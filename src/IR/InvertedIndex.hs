module IR.InvertedIndex where 

import qualified Data.HashMap.Strict as Map
import Data.List (intercalate, nub)
import qualified Data.Set as Set
import IR.TermDocument
import Parser.BoolQueryParser
import Data.List as List

type InvertedIndex = Map.HashMap Term [Document]

buildInvertedIndex :: [(Document, String)] -> InvertedIndex
buildInvertedIndex documents = foldl (\acc (document, content) -> insertTerms document content acc ) Map.empty documents
  where
    insertTerms document content index  = 
        foldl (\acc term -> Map.insertWith (++) term [document] acc ) index (documentTerms content)
      

hasTermDocument :: Term -> Document -> InvertedIndex -> Bool
hasTermDocument term document index =
    case Map.lookup term index of
        Nothing -> False 
        Just docs -> elem document docs

executeQuery :: InvertedIndex -> Expr -> [Document]
executeQuery index (Term term) = Map.lookupDefault [] term index
executeQuery index (And q1 q2) =
    List.intersect (executeQuery index q1) (executeQuery index q2)
executeQuery index (Or q1 q2) =
    List.union (executeQuery index q1) (executeQuery index q2)
executeQuery index (Not q) =
    let allDocs = nub $ concat $ Map.elems index
        queryDocs = Set.fromList $ executeQuery index q
    in  filter (`Set.notMember` queryDocs) allDocs

-- Given a string query, execute it against an inverted index.
search :: InvertedIndex -> String -> [Document]
search index query = 
  case parseExpr query of
    Left _ -> []
    Right expr -> executeQuery index expr
    
-- Convert InvertedIndex to list of strings
invertedIndexToStrings :: InvertedIndex -> [String]
invertedIndexToStrings index = do
    (term, docs) <- Map.toList index
    let docsStr = intercalate ", " docs
    return $ term ++ ": " ++ docsStr