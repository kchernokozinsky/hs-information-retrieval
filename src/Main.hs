import IOUtils
import qualified Data.Set as Set
import Text.Regex.TDFA ((=~), getAllTextMatches, AllTextMatches)
import IncidenceMatrix as Matrix
import InvertedIndex as Index
import QueryParser

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

    -- mapM_ (\(fileName, content) -> writeStringToFile ("resources/out/" ++ fileName) content) fileContentPairs
    print "done"
    

