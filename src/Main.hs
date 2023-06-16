import Utils.IO
import qualified Data.Set as Set
import Text.Regex.TDFA ((=~), getAllTextMatches, AllTextMatches)
import IR.IncidenceMatrix as Matrix
import IR.InvertedIndex as Index
import IR.CoordinateInvertedIndex as CoordIndex
import Parser.CoordQueryParser as CoordParser
import qualified Data.HashMap.Strict as Map


getWords :: String -> [String]
getWords str = getAllTextMatches (str =~ "[a-zA-Z0-9']+" :: AllTextMatches [] String)

uniqueWords :: [String] -> Set.Set String

uniqueWords contents = foldl (\acc content -> acc `Set.union` Set.fromList (getWords content)) Set.empty contents

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
    in  Map.keys docs'

main :: IO ()
main = do

    -- READ FILES --

    let folder = "resources/fb2"
    fileNames <- getFileNames folder
    contents <- mapM readFileWithEncoding $ map ((folder ++ "/") ++) fileNames
    let fileContentPairs = zip fileNames contents

    -- CREATE SET --

    let wordsSet = uniqueWords contents
    writeSetToFile "resources/out/set.txt" wordsSet

    -- CREATE INCIDENCE MATRIX TERM - DOCUMENT --

    let incidenceMatrix = buildIncidenceMatrix fileContentPairs
    writeIncidenceMatrixToFile "resources/out/incidence-matrix.txt" incidenceMatrix
    print $ Matrix.search incidenceMatrix "apple AND veil"

    -- CREATE INVERTED INDEX --

    let invertedIndex = buildInvertedIndex fileContentPairs
    writeInvertedIndexToFile "resources/out/inverted-index.txt" invertedIndex
    print $ Index.search invertedIndex "apple AND veil"

    -- CREATE COORDINATE INVERTED INDEX --

    let coordinateInvertedIndex = buildCoordinateInvertedIndex fileContentPairs
    writeCoordinateInvertedIndexToFile "resources/out/coord-inverted-index.txt" coordinateInvertedIndex
    print $ CoordIndex.search coordinateInvertedIndex "cowbellied\\{2}Thou"

    -- mapM_ (\(fileName, content) -> writeStringToFile ("resources/out/" ++ fileName) content) fileContentPairs
    print "done"
    

