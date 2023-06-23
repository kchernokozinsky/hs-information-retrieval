module IR.KGramIndex where

import qualified IR.InvertedIndex as II
import Data.List.Split
type KGramIndex = II.InvertedIndex

buildKGramIndex :: [String] -> KGramIndex
buildKGramIndex terms = II.buildInvertedIndex $ termsToPairs terms

termToThreeGrams :: String -> [String]
termToThreeGrams term = go ("$" ++ term ++ "$")
    where go (x:y:z:xs) = [x,y,z] : go (y:z:xs)
          go _ = []

termsToPairs :: [String] -> [(String, String)]
termsToPairs terms = foldl (\acc term -> acc ++ (foldl(\acc grm -> acc ++ [(term, grm)])[] (termToThreeGrams term))) [] terms

search :: KGramIndex -> String -> [String]
search index query = II.search index query''
    where query' = if (head query) == '*' && (last query) == '*' then query 
                   else if (head query) == '*' then (tail query) ++ "$"
                   else if (last query) == '*' then "$" ++ (init query)
                   else "$" ++ query ++ "$"
          split = splitOn "*" query'
          kgrams = foldl (\acc term -> acc ++ termToThreeGrams term) [] split
          query'' = foldl (\acc term -> acc ++ " AND " ++ term) (head kgrams) (tail kgrams)
          termToThreeGrams term = go term
            where go (x:y:z:xs) = [x,y,z] : go (y:z:xs)
                  go _ = []