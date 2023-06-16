module Parser.CoordQueryParser (parseTermDistancePairs, TermDistancePair(TermDistancePair)) where 

import IR.TermDocument
import Text.Megaparsec
import Text.Megaparsec.Char
import Text.Megaparsec.Char.Lexer (decimal)
import Data.Void
import Control.Monad.Combinators.Expr
import Data.Text (Text)

data TermDistancePair = TermDistancePair Term Distance Term deriving (Show)

type Parser = Parsec Void String

term :: Parser Term
term = manyTill anySingle (lookAhead $ string "\\{" <|> (eof *> return ""))

distance :: Parser Distance
distance = do
    _ <- string "\\{"
    n <- decimal
    _ <- char '}'
    return n

termDistancePair :: Parser TermDistancePair
termDistancePair = do
    t1 <- term
    d <- distance
    t2 <- term
    return $ TermDistancePair t1 d t2

termDistancePairs :: Parser [TermDistancePair]
termDistancePairs = many termDistancePair

parseTermDistancePairs :: String -> Either (ParseErrorBundle String Void) [TermDistancePair]
parseTermDistancePairs = parse (termDistancePairs <* eof) ""


