module IR.IncidenceMatrix where 

import qualified Data.HashMap.Strict as Map
import Data.List (intercalate, nub)
import IR.TermDocument
import Data.List as List
import qualified Data.Set as Set
import Parser.BoolQueryParser


type IncidenceMatrix = Map.HashMap Term (Map.HashMap Document Bool)

emptyIncidenceMatrix :: IncidenceMatrix
emptyIncidenceMatrix = Map.empty

insertTermDocument :: Term -> Document -> IncidenceMatrix -> IncidenceMatrix
insertTermDocument term document matrix =
    Map.insertWith Map.union term (Map.singleton document True) matrix


hasTermDocument :: Term -> Document -> IncidenceMatrix -> Bool
hasTermDocument term document matrix =
    case Map.lookup term matrix of
        Nothing -> False 
        Just docs -> case Map.lookup document docs of
            Nothing -> False
            Just _ -> True


buildIncidenceMatrix :: [(Document, String)] -> IncidenceMatrix
buildIncidenceMatrix documents = foldl(\acc (document, content) -> insertTermsFromDocument document content acc ) emptyIncidenceMatrix documents
    where insertTermsFromDocument document content matrix = foldl(\acc term -> insertTermDocument term document acc ) matrix (documentTerms content)

allTermDocumentPairs :: IncidenceMatrix -> [(Term, Document)]
allTermDocumentPairs matrix = do
    let allTerms = Map.keys matrix
    let allDocs = nub $ concatMap Map.keys (Map.elems matrix)
    term <- allTerms
    doc <- allDocs
    return (term, doc)

executeQuery :: IncidenceMatrix -> Expr -> [Document]
executeQuery matrix (Term term) = 
    case Map.lookup term matrix of
        Just docs -> Map.keys $ Map.filter id docs
        Nothing -> []
executeQuery matrix (And q1 q2) = 
    List.intersect 
    (executeQuery matrix q1) 
    (executeQuery matrix q2)
executeQuery matrix (Or q1 q2) = 
    List.union 
    (executeQuery matrix q1) 
    (executeQuery matrix q2)
executeQuery matrix (Not q) = 
    let allDocs = nub $ concatMap Map.keys (Map.elems matrix)
        queryDocs = Set.fromList $ executeQuery matrix q
    in  filter (`Set.notMember` queryDocs) allDocs

search :: IncidenceMatrix -> String -> [Document]
search matrix query = 
  case parseExpr query of
    Left _ -> []
    Right expr -> executeQuery matrix expr

incidenceMatrixToStrings :: IncidenceMatrix -> [String]
incidenceMatrixToStrings matrix = do
    (term, doc) <- allTermDocumentPairs matrix
    let present = maybe False id (Map.lookup doc =<< Map.lookup term matrix)
    return $ intercalate ", " [term, doc, show present]

