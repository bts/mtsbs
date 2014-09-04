module Transformers where

import Data.Maybe
import qualified Data.Map as Map
import Control.Monad.Identity
--import Control.Monad.Except
--import Control.Monad.Reader
--import Control.Monad.State
--import Control.Monad.Writer

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

-- non-monadic "reference impl" evaluator:
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
