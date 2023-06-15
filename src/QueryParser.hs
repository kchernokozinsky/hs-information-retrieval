module QueryParser where

import Text.Megaparsec
import Text.Megaparsec.Char
import qualified Text.Megaparsec.Char.Lexer as L
import Control.Monad ()
import Data.Void
import Control.Monad.Combinators.Expr

type Parser = Parsec Void String

data Expr
  = Term String
  | Not Expr
  | And Expr Expr
  | Or Expr Expr
  deriving (Show)

sc :: Parser ()
sc = L.space space1 empty empty

lexeme :: Parser a -> Parser a
lexeme = L.lexeme sc

symbol :: String -> Parser String
symbol = L.symbol sc

term :: Parser Expr
term = Term <$> lexeme (some alphaNumChar)

notExpr :: Parser (Expr -> Expr)
notExpr = Not <$ symbol "NOT"

andExpr :: Parser (Expr -> Expr -> Expr)
andExpr = And <$ symbol "AND"

orExpr :: Parser (Expr -> Expr -> Expr)
orExpr = Or <$ symbol "OR"

operators :: [[Operator Parser Expr]]
operators =
  [ [ Prefix notExpr ]
  , [ InfixL andExpr, InfixL orExpr ]
  ]

expr :: Parser Expr
expr = makeExprParser term operators

parseExpr :: String -> Either (ParseErrorBundle String Void) Expr
parseExpr = parse (expr <* eof) ""
