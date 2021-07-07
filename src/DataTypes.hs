module DataTypes (LispExpr (..)) where

type Number = Int

type Identifier = String

data LispExpr
  = LispNumber Number
  | LispString String
  | LispBool Bool
  | LispIdent Identifier
  | LispLambda [String] LispExpr -- Bindings and expression using those identifiers
  | LispProcedure {run :: [LispExpr] -> LispExpr}
  | LispLet String LispExpr LispExpr
  | LispList [LispExpr]

instance Show LispExpr where
  show (LispNumber n) = show n
  show (LispString str) = show str
  show (LispBool bool) = show bool
  show (LispIdent ident) = ident
  show (LispLet binding val expr) = "[Let " ++ binding ++ " " ++ show val ++ " " ++ show expr ++ "]"
  show (LispLambda bindings expr) = "[Lambda " ++ (unwords . map show) bindings ++ " " ++ (show expr) ++ "]"
  show (LispProcedure proc) = "[LispProcedure]"
  show (LispList exprs) = "(" ++ (unwords . map show) exprs ++ ")"

instance Eq LispExpr where
  (==) (LispNumber x) (LispNumber y) = (==) x y
  (==) (LispString x) (LispString y) = (==) x y
  (==) (LispBool x) (LispBool y) = (==) x y
  (==) (LispIdent x) (LispIdent y) = (==) x y
  (==) _ _ = False

