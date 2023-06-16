module Parser.DoubleBoolQueryParser (parseCompoundTerm) where

import Text.Megaparsec
import Text.Megaparsec.Char
import Control.Monad
import Data.Void
import qualified Parser.BoolQueryParser as P

type Parser = Parsec Void String

term :: Parser P.Expr
term = P.Term <$> manyTill anySingle (try $ lookAhead $ eof <|> void (chunk " AND "))

andOp :: Parser (P.Expr -> P.Expr -> P.Expr)
andOp = string " AND " *> pure P.And

compoundTerm :: Parser P.Expr
compoundTerm = do
    firstTerm <- term
    rest <- many $ andOp *> term
    return $ foldl P.And firstTerm rest

parseCompoundTerm :: String -> Either (ParseErrorBundle String Void) P.Expr
parseCompoundTerm = parse (compoundTerm <* eof) ""
