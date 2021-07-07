module Evaluator (eval) where

import DataTypes (LispExpr (..))
import Environment as E

eval :: E.Env -> LispExpr -> LispExpr
-- Evaluate identifier by looking up the value in the current environment (and
-- evaluating it)
eval env (LispIdent ident) = case E.lookup env ident of
  Just expr -> eval env expr
  Nothing -> error $ "\nError: Unbound identifier: " ++ ident ++ "\n"
-- Evaluate a lambda by generating a procedure that will bind the arguments to
-- the identifiers and evaluate the expression with this new environment
eval env (LispLambda bindings expr) = LispProcedure $ \args ->
  let env' = E.merge env $ E.create (zip bindings args)
   in eval env' expr
-- Evaluate let-binding by pushing a bound value to the environment
eval env (LispLet binding val expr) =
  let env' = push env (binding, val)
   in eval env' expr
-- Evaluate list by recursively evaluating all the expressions and then calling
-- the car of the list with the cdr as arguments
eval env (LispList []) = error "Cannot apply: missing procedure"
eval env (LispList exprs) = apply proc args
  where
    (proc : args) = map (eval env) exprs
-- Evaluate atom as itself
eval _ atom = atom

apply :: LispExpr -> [LispExpr] -> LispExpr
apply (LispProcedure proc) args = proc args
apply expr _ = error $ "Cannot apply: " ++ show expr ++ " is not callable."
