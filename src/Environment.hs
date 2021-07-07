module Environment (lookup, push, merge, create, prelude, Env) where

import DataTypes (LispExpr (..))
import Prelude hiding (lookup)

type Env = [(String, LispExpr)]

lookup :: Env -> String -> Maybe LispExpr
lookup [] query = Nothing
lookup ((ident, expr) : env') query =
  if ident == query
    then Just expr
    else lookup env' query

push :: Env -> (String, LispExpr) -> Env
push = flip (:)

merge :: Env -> Env -> Env
merge = (++)

create :: [(String, LispExpr)] -> Env
create = id

--
-- Builtin procedures
--

plus :: [LispExpr] -> LispExpr
plus [] = error "Not enough arguments supplied"
plus (expr : exprs) = foldl add2 expr exprs
  where
    add2 (LispNumber n) (LispNumber m) = LispNumber (n + m)
    add2 _ _ = error "Type error: Can only add together numbers."

minus :: [LispExpr] -> LispExpr
minus [] = error "Not enough arguments supplied"
minus (expr : exprs) = foldl subtract2 expr exprs
  where
    subtract2 (LispNumber n) (LispNumber m) = LispNumber (n - m)
    subtract2 _ _ = error "Type error: Can only subtract numbers."

mult :: [LispExpr] -> LispExpr
mult [] = error "Not enough arguments supplied"
mult (expr : exprs) = foldl mult2 expr exprs
  where
    mult2 (LispNumber n) (LispNumber m) = LispNumber (n * m)
    mult2 _ _ = error "Type error: Can only subtract numbers."

ifThen :: [LispExpr] -> LispExpr
ifThen ((LispBool antecedent) : consequent : alternative : []) =
  if antecedent then consequent else alternative
ifThen exprs = error $ "Could not evaluate if-condition with arguments " ++ show exprs

equals :: [LispExpr] -> LispExpr
equals [] = error "Not enough arguments supplied"
equals (expr : exprs) = 
   LispBool $ foldr (\val acc -> acc && val == expr) True exprs

prelude :: Env

prelude =
  [ ("+", LispProcedure plus),
    ("-", LispProcedure minus),
    ("*", LispProcedure mult),
    ("=", LispProcedure equals),
    ("if", LispProcedure ifThen)
  ]
