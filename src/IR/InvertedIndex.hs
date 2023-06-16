{-# LANGUAGE DeriveGeneric #-}

module IR.InvertedIndex where 

import qualified Data.HashMap.Strict as HashMap
import qualified Data.Map as Map
import Data.List (intercalate, nub)
import qualified Data.Set as Set
import IR.TermDocument
import Parser.BoolQueryParser
import Data.List as List

type InvertedIndex = HashMap.HashMap Term [Document]

buildInvertedIndex :: [(Document, String)] -> InvertedIndex
buildInvertedIndex documents = foldl (\acc (document, content) -> insertTerms document content acc ) HashMap.empty documents
  where
    insertTerms document content index  = 
        foldl (\acc term -> HashMap.insertWith (\new old -> nub (new ++ old)) term [document] acc ) index (documentTerms content)
      

hasTermDocument :: Term -> Document -> InvertedIndex -> Bool
hasTermDocument term document index =
    case HashMap.lookup term index of
        Nothing -> False 
        Just docs -> elem document docs

executeQuery :: InvertedIndex -> Expr -> [Document]
executeQuery index (Term term) = HashMap.lookupDefault [] term index
executeQuery index (And q1 q2) =
    List.intersect (executeQuery index q1) (executeQuery index q2)
executeQuery index (Or q1 q2) =
    List.union (executeQuery index q1) (executeQuery index q2)
executeQuery index (Not q) =
    let allDocs = nub $ concat $ HashMap.elems index
        queryDocs = Set.fromList $ executeQuery index q
    in  filter (`Set.notMember` queryDocs) allDocs

search :: InvertedIndex -> String -> [Document]
search index query = 
  case parseExpr query of
    Left _ -> []
    Right expr -> executeQuery index expr
    
invertedIndexToStrings :: InvertedIndex -> [String]
invertedIndexToStrings index = do
    (term, docs) <- HashMap.toList index
    let docsStr = intercalate ", " docs
    return $ term ++ ": " ++ docsStr

