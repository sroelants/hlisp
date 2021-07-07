{-# LANGUAGE FlexibleInstances #-}

module Parser where

import Control.Applicative
import Data.Char
import DataTypes (LispExpr (..))
import Prelude

--
-- Data declarations
--

newtype Parser a = Parser {runParser :: String -> Maybe (a, String)}

--
-- Typeclass instances
--

instance Functor Parser where
  fmap f (Parser p) = Parser $ \input -> do
    (a, input') <- p input
    return (f a, input')

instance Applicative Parser where
  pure a = Parser $ \input -> Just (a, input)

  (Parser p1) <*> Parser p2 = Parser $ \input -> do
    (f, input') <- p1 input
    (a, input'') <- p2 input'
    return (f a, input'')

instance Alternative Parser where
  empty = Parser $ const Nothing
  (Parser p1) <|> (Parser p2) = Parser $ \input -> p1 input <|> p2 input

instance Monad Parser where
  (Parser pa) >>= f = Parser $ \input -> do
    (a, input') <- pa input
    runParser (f a) input'

instance (Semigroup a) => Semigroup (Parser a) where
  (<>) = liftA2 (<>)

instance MonadFail Parser where
  fail _ = empty

--
-- Parsers
--
predP :: (Char -> Bool) -> Parser Char
predP pred = Parser f
  where
    f (x : xs) | pred x = Just (x, xs)
    f _ = Nothing

charP :: Char -> Parser Char
charP c = predP (== c)

stringP :: String -> Parser String
stringP = traverse charP

notInP :: [Char] -> Parser Char
notInP cs = predP (`notElem` cs)

sepByP :: Parser a -> Parser b -> Parser [b]
sepByP sep elem = (:) <$> elem <*> (many (sep *> elem)) <|> pure []

--
-- Numeric parsers
--
numeric :: Parser Char
numeric = predP isDigit

digit :: Parser Int
digit = read . return <$> numeric

nat :: Parser Int
nat = read <$> some numeric

neg :: Parser Int
neg = read <$> stringP "-" <> some numeric

int :: Parser Int
int = nat <|> neg

--
-- String parsers
--
quote :: Parser Char
quote = charP '\"'

openParen :: Parser Char
openParen = charP '('

closeParen :: Parser Char
closeParen = charP ')'

string :: Parser String
string = many $ predP (/= '\"')

whitespace :: Parser String
whitespace = some $ charP ' '

stringLit :: Parser String
stringLit = quote *> string <* quote

--
-- Lisp parsers
--
parseLisp :: Parser LispExpr
parseLisp =
  lispNumber
    <|> lispString
    <|> lispBool
    <|> lispIdent
    <|> lispLet
    <|> lispLambda
    <|> lispList

lispExpr = parseLisp

lispNumber :: Parser LispExpr
lispNumber = LispNumber <$> int

lispString :: Parser LispExpr
lispString = LispString <$> stringLit

lispIdent :: Parser LispExpr
lispIdent = LispIdent <$> some (notInP [' ', '\'', '(', ')'])

lispBool :: Parser LispExpr
lispBool = LispBool . toBool <$> (stringP "#t" <|> stringP "#f")
  where
    toBool "#f" = False
    toBool _ = True

lispList :: Parser LispExpr
lispList = LispList <$> (openParen *> sepByP whitespace lispExpr <* closeParen)

lispLambda :: Parser LispExpr
lispLambda = do
  (LispList bindings) <- openParen *> stringP "lambda" *> whitespace *> lispList <* whitespace
  expr <- lispExpr <* closeParen
  return $ LispLambda (map show bindings) expr

lispLet :: Parser LispExpr
lispLet = do
  (LispIdent binding) <- openParen *> stringP "let" *> whitespace *> lispIdent <* whitespace
  value <- lispExpr <* whitespace
  expr <- lispExpr <* closeParen
  return $ LispLet binding value expr

parse :: String -> LispExpr
parse input = case runParser parseLisp input of
  Just (expr, _) -> expr
  Nothing -> error "Failed to parse"
