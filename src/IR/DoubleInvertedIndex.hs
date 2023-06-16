module IR.DoubleInvertedIndex where 

import qualified Data.HashMap.Strict as Map
import Data.List (intercalate, nub)
import qualified Data.Set as Set
import IR.TermDocument
import Parser.DoubleBoolQueryParser
import Data.List as List
import IR.InvertedIndex

type DoubleInvertedIndex = Map.HashMap DoubleTerm [Document]

buildDoubleInvertedIndex :: [(Document, String)] -> DoubleInvertedIndex
buildDoubleInvertedIndex documents = foldl (\acc (document, content) -> insertTerms document content acc ) Map.empty documents
  where
    insertTerms document content index  = 
        foldl (\acc term -> Map.insertWith (++) term [document] acc ) index (documentTerms'' content)
      

hasTermDocument :: DoubleTerm -> Document -> DoubleInvertedIndex -> Bool
hasTermDocument term document index =
    case Map.lookup term index of
        Nothing -> False 
        Just docs -> elem document docs


-- Given a string query, execute it against an inverted index.
search :: DoubleInvertedIndex -> String -> [Document]
search index query = 
    case parseCompoundTerm query' of
        Left _ -> [] 
        Right q -> executeQuery index q
    where 
        query' = foldl (\acc t -> acc ++ " AND " ++ t ) (head doubleTerms) (tail doubleTerms)
        doubleTerms = documentTerms'' query

    
-- Convert InvertedIndex to list of strings
invertedIndexToStrings :: DoubleInvertedIndex -> [String]
invertedIndexToStrings index = do
    (term, docs) <- Map.toList index
    let docsStr = intercalate ", " docs
    return $ term ++ ": " ++ docsStr