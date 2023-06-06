import IOUtils
import qualified Data.Set as Set
import Text.Regex.TDFA ((=~), getAllTextMatches, AllTextMatches)

getWords :: String -> [String]
getWords str = getAllTextMatches (str =~ "[a-zA-Z0-9']+" :: AllTextMatches [] String)

uniqueWords :: [String] -> Set.Set String
uniqueWords contents = foldl (\acc content -> acc `Set.union` Set.fromList (getWords content)) Set.empty contents

main :: IO ()
main = do
    let folder = "resources/fb2"
    fileNames <- getFileNames folder
    contents <- mapM readFileWithEncoding $ map ((folder ++ "/") ++) fileNames
    let fileContentPairs = zip fileNames contents
    let wordsSet = uniqueWords contents
    -- mapM_ (\(fileName, content) -> writeStringToFile ("resources/out/" ++ fileName) content) fileContentPairs
    writeSetToFile "resources/out/set.txt" wordsSet 
    print "done"

