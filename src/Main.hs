import Utils.IO
import qualified Data.Set as Set
import Text.Regex.TDFA ((=~), getAllTextMatches, AllTextMatches)
import IR.IncidenceMatrix as Matrix
import IR.InvertedIndex as Index
import IR.CoordinateInvertedIndex as CoordIndex
import IR.DoubleInvertedIndex as DoubleIndex


getWords :: String -> [String]
getWords str = getAllTextMatches (str =~ "[a-zA-Z0-9']+" :: AllTextMatches [] String)

uniqueWords :: [String] -> Set.Set String

uniqueWords contents = foldl (\acc content -> acc `Set.union` Set.fromList (getWords content)) Set.empty contents

main :: IO ()
main = do

    -- READ FILES --

    let folder = "resources/fb2"
    fileNames <- getFileNames folder
    contents <- mapM readFileWithEncoding $ map ((folder ++ "/") ++) fileNames
    let fileContentPairs = zip fileNames contents

    -- -- CREATE SET --

    let wordsSet = uniqueWords contents
    writeSetToFile "resources/out/set.txt" wordsSet

    -- -- CREATE INCIDENCE MATRIX TERM - DOCUMENT --

    let incidenceMatrix = buildIncidenceMatrix fileContentPairs
    writeIncidenceMatrixToFile "resources/out/incidence-matrix.txt" incidenceMatrix
    print $ Matrix.search incidenceMatrix "apple AND veil"

    -- -- CREATE INVERTED INDEX --

    let invertedIndex = buildInvertedIndex fileContentPairs
    writeInvertedIndexToFile "resources/out/inverted-index.txt" invertedIndex
    print $ Index.search invertedIndex "apple AND veil"

    -- -- CREATE COORDINATE INVERTED INDEX --

    let coordinateInvertedIndex = buildCoordinateInvertedIndex fileContentPairs
    writeCoordinateInvertedIndexToFile "resources/out/coord-inverted-index.txt" coordinateInvertedIndex
    print $ CoordIndex.search coordinateInvertedIndex "cowbellied\\{2}Thou"

    -- -- CREATE DOUBLE INVERTED INDEX --

    let doubleInvertedIndex = buildDoubleInvertedIndex fileContentPairs
    writeInvertedIndexToFile "resources/out/double-inverted-index.txt" doubleInvertedIndex
    print $ DoubleIndex.search doubleInvertedIndex "Douglas coming down the stair"

    print "done"
    

