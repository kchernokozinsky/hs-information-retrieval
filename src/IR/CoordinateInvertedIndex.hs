module IR.CoordinateInvertedIndex where 

import qualified Data.HashMap.Strict as Map
import Data.List (intercalate, nub)
import qualified Data.Set as Set
import IR.TermDocument
import Data.List as List
import Parser.CoordQueryParser

type CoordinateInvertedIndex = Map.HashMap Term (Map.HashMap Document [Coord])

buildCoordinateInvertedIndex :: [(Document, String)] -> CoordinateInvertedIndex
buildCoordinateInvertedIndex documents = foldl (\acc (document, content) -> insertTerms document content acc ) Map.empty documents
  where
    insertTerms document content index  = 
        foldl (\acc (term, coords) -> Map.insertWith Map.union term (Map.singleton document coords) acc ) index (documentTerms' content)
      
executeQuery :: CoordinateInvertedIndex -> [TermDistancePair] -> [Document]
executeQuery index [] = []
executeQuery index (TermDistancePair t1 d t2:rest) = 
    let docs1 = Map.lookupDefault Map.empty t1 index
        docs2 = Map.lookupDefault Map.empty t2 index
        docs = Map.intersectionWith (++) docs1 docs2
        hasDistance d doc = 
          let coords1 = Map.lookupDefault [] doc docs1
              coords2 = Map.lookupDefault [] doc docs2
              coords = [(c1, c2) | c1 <- coords1, c2 <- coords2, abs (c1 - c2) <= d]
          in  not $ null coords
        docs' = Map.filterWithKey (\k _ -> hasDistance d k) docs
    in  Map.keys docs' ++ executeQuery index rest
 
-- Given a string query, execute it against an inverted index.
search :: CoordinateInvertedIndex -> String -> [Document]
search index query = 
  case parseTermDistancePairs query of
    Left _ ->[] 
    Right query -> executeQuery index query
    
-- Convert InvertedIndex to list of strings
coordinateInvertedIndexToStrings :: CoordinateInvertedIndex -> [String]
coordinateInvertedIndexToStrings index = do
    (term, docs) <- Map.toList index
    let docsStrs = do
          (doc, coords) <- Map.toList docs
          let coordsStr = intercalate ", " (map show coords)
          return $ doc ++ ": [" ++ coordsStr ++ "]"
    let docsStr = intercalate ", " docsStrs
    return $ term ++ ": {" ++ docsStr ++ "}"