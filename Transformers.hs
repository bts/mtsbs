module Transformers where

import Data.Maybe
import qualified Data.Map as Map
import Control.Monad.Identity
import Control.Monad.Except
import Control.Monad.Reader
import Control.Monad.State
import Control.Monad.Writer

type Name = String

data Exp = Lit Integer
         | Var Name
         | Plus Exp Exp
         | Abs Name Exp
         | App Exp Exp
         deriving (Show)

data Value = IntVal Integer
           | FunVal Env Name Exp
           deriving (Show)

type Env = Map.Map Name Value

-- eval0, a non-monadic "reference impl" evaluator:
-- intentionally superfluous for comparison with forthcoming monadic version

eval0 :: Env -> Exp -> Value
eval0 _ (Lit i) = IntVal i
eval0 env (Var n) = fromJust (Map.lookup n env)
eval0 env (Plus e1 e2) = let IntVal i1 = eval0 env e1
                             IntVal i2 = eval0 env e2
                         in IntVal (i1 + i2)
eval0 env (Abs n e) = FunVal env n e
eval0 env (App e1 e2) =
  let v1 = eval0 env e1
      v2 = eval0 env e2
  in case v1 of
      FunVal env' n body -> eval0 (Map.insert n v2 env') body

-- exampleExp = Lit 12 `Plus` (App (Abs "x" (Var "x")) (Lit 4 `Plus` Lit 2))
-- eval0 Map.empty exampleExp

-- eval1, a monadic evaluator:

type Eval1 a = Identity a

runEval1 :: Eval1 a -> a
runEval1 = runIdentity

eval1 :: Env -> Exp -> Eval1 Value
eval1 _ (Lit i) = return $ IntVal i
eval1 env (Var n) = return $ fromJust (Map.lookup n env)
eval1 env (Plus e1 e2) = do IntVal i1 <- eval1 env e1
                            IntVal i2 <- eval1 env e2
                            return $ IntVal (i1 + i2)
eval1 env (Abs n e) = return $ FunVal env n e
eval1 env (App e1 e2) =
  do v1 <- eval1 env e1
     v2 <- eval1 env e2
     case v1 of
      FunVal env' n body -> eval1 (Map.insert n v2 env') body

-- exampleExp = Lit 12 `Plus` (App (Abs "x" (Var "x")) (Lit 4 `Plus` Lit 2))
-- runEval1 $ eval1 Map.empty exampleExp

-- eval2, an evaluator using the ExceptT monad transformer to extend our basic
-- Eval1 monad to handle exceptions:

type Eval2 a = ExceptT String Identity a

runEval2 :: Eval2 a -> Either String a
runEval2 = runIdentity . runExceptT

eval2 :: Env -> Exp -> Eval2 Value
eval2 _ (Lit i) = return $ IntVal i
eval2 env (Var n) = case Map.lookup n env of
                     Just v -> return v
                     Nothing -> throwError ("unbound variable: " ++ n)
eval2 env (Plus e1 e2) = do v1 <- eval2 env e1
                            v2 <- eval2 env e2
                            case (v1, v2) of
                             (IntVal i1, IntVal i2) -> return $ IntVal (i1 + i2)
                             _ -> throwError "type error in addition"
eval2 env (Abs n e) = return $ FunVal env n e
eval2 env (App e1 e2) =
  do v1 <- eval2 env e1
     v2 <- eval2 env e2
     case v1 of
      FunVal env' n body -> eval2 (Map.insert n v2 env') body
      _ -> throwError "type error in application"

-- eval3, using the ReaderT transformer to extend our monad with an environment

type Eval3 a = ReaderT Env (ExceptT String Identity) a

runEval3 :: Env -> Eval3 a -> Either String a
runEval3 env ev = runIdentity $ runExceptT $ runReaderT ev env

eval3 :: Exp -> Eval3 Value
eval3 (Lit i) = return $ IntVal i
eval3 (Var n) = do mv <- reader $ Map.lookup n
                   case mv of
                    Just v -> return v
                    Nothing -> throwError $ "unbound variable: " ++ n
eval3 (Plus e1 e2) = do v1 <- eval3 e1
                        v2 <- eval3 e2
                        case (v1, v2) of
                         (IntVal i1, IntVal i2) -> return $ IntVal (i1 + i2)
                         _ -> throwError "type error in addition"
eval3 (Abs n e) = do env <- ask
                     return $ FunVal env n e
eval3 (App e1 e2) =
  do v1 <- eval3 e1
     v2 <- eval3 e2
     case v1 of
      FunVal env' n body -> local (const (Map.insert n v2 env')) $ eval3 body
      _ -> throwError "type error in application"

-- eval4, with StateT to keep track of "profiling" state

type Eval4 a = ReaderT Env (ExceptT String (StateT Integer Identity)) a

runEval4 :: Env -> Integer -> Eval4 a -> (Either String a, Integer)
runEval4 env st ev = runIdentity $ runStateT (runExceptT (runReaderT ev env)) st

tick :: (Num s, MonadState s m) => m ()
tick = do st <- get
          put (st + 1)

eval4 :: Exp -> Eval4 Value
eval4 (Lit i) = do tick
                   return $ IntVal i
eval4 (Var n) = do tick
                   mv <- reader $ Map.lookup n
                   case mv of
                    Just v -> return v
                    Nothing -> throwError $ "unbound variable: " ++ n
eval4 (Plus e1 e2) = do tick
                        v1 <- eval4 e1
                        v2 <- eval4 e2
                        case (v1, v2) of
                         (IntVal i1, IntVal i2) -> return $ IntVal (i1 + i2)
                         _ -> throwError "type error in addition"
eval4 (Abs n e) = do tick
                     env <- ask
                     return $ FunVal env n e
eval4 (App e1 e2) =
  do tick
     v1 <- eval4 e1
     v2 <- eval4 e2
     case v1 of
      FunVal env' n body -> local (const (Map.insert n v2 env')) $ eval4 body
      _ -> throwError "type error in application"

-- let exampleExp = Lit 12 `Plus` (App (Abs "x" (Var "x")) (Lit 4 `Plus` Lit 2))
-- runEval4 Map.empty 0 $ eval4 exampleExp

-- eval5, with WriterT to log encountered variable names

type Eval5 a = ReaderT Env (ExceptT String (WriterT [String] (StateT Integer Identity))) a

runEval5 :: Env -> Integer -> Eval5 a -> ((Either String a, [String]), Integer)
runEval5 env st ev = runIdentity $
                     runStateT (runWriterT (runExceptT (runReaderT ev env))) st

eval5 :: Exp -> Eval5 Value
eval5 (Lit i) = do tick
                   return $ IntVal i
eval5 (Var n) = do tick
                   tell [n]
                   mv <- reader $ Map.lookup n
                   case mv of
                    Just v -> return v
                    Nothing -> throwError $ "unbound variable: " ++ n
eval5 (Plus e1 e2) = do tick
                        v1 <- eval5 e1
                        v2 <- eval5 e2
                        case (v1, v2) of
                         (IntVal i1, IntVal i2) -> return $ IntVal (i1 + i2)
                         _ -> throwError "type error in addition"
eval5 (Abs n e) = do tick
                     env <- ask
                     return $ FunVal env n e
eval5 (App e1 e2) =
  do tick
     v1 <- eval5 e1
     v2 <- eval5 e2
     case v1 of
      FunVal env' n body -> local (const (Map.insert n v2 env')) $ eval5 body
      _ -> throwError "type error in application"

-- let exampleExp = Lit 12 `Plus` (App (Abs "x" (Var "x")) (Lit 4 `Plus` Lit 2))
-- runEval5 Map.empty 0 $ eval5 exampleExp

-- eval6, which replaces Identity with IO so we can add printlines

type Eval6 a = ReaderT Env (ExceptT String (WriterT [String] (StateT Integer IO))) a

runEval6 :: Env -> Integer -> Eval6 a -> IO ((Either String a, [String]), Integer)
runEval6 env st ev = runStateT (runWriterT (runExceptT (runReaderT ev env))) st

eval6 :: Exp -> Eval6 Value
eval6 (Lit i) = do tick
                   liftIO $ print i
                   return $ IntVal i
eval6 (Var n) = do tick
                   tell [n]
                   mv <- reader $ Map.lookup n
                   case mv of
                    Just v -> return v
                    Nothing -> throwError $ "unbound variable: " ++ n
eval6 (Plus e1 e2) = do tick
                        v1 <- eval6 e1
                        v2 <- eval6 e2
                        case (v1, v2) of
                         (IntVal i1, IntVal i2) -> return $ IntVal (i1 + i2)
                         _ -> throwError "type error in addition"
eval6 (Abs n e) = do tick
                     env <- ask
                     return $ FunVal env n e
eval6 (App e1 e2) =
  do tick
     v1 <- eval6 e1
     v2 <- eval6 e2
     case v1 of
      FunVal env' n body -> local (const (Map.insert n v2 env')) $ eval6 body
      _ -> throwError "type error in application"

-- let exampleExp = Lit 12 `Plus` (App (Abs "x" (Var "x")) (Lit 4 `Plus` Lit 2))
-- runEval6 Map.empty 0 $ eval6 exampleExp
