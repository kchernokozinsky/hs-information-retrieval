import System.Directory
import System.IO
import Text.Regex
import qualified Data.Text as T
import qualified Data.Set as Set
import Text.Regex.TDFA ((=~), getAllTextMatches, AllTextMatches)

trim :: String -> String
trim = T.unpack . T.strip . T.pack

getFileNames :: FilePath -> IO [FilePath]
getFileNames dir = do
    allNames <- getDirectoryContents dir
    return $ filter (`notElem` [".", ".."]) allNames

readFileWithEncoding :: FilePath -> IO String
readFileWithEncoding filePath = do
  h <- openFile filePath ReadMode
  hSetEncoding h latin1
  content <- hGetContents h
  return $ processFile content

processFile :: String -> String
processFile str = 
    let noTags = subRegex (mkRegex "<[^>]*>") str ""
        noSpecialChars = subRegex (mkRegex "[^a-zA-Z0-9 ,.;:'`\"!?]") noTags "" :: String
    in trim noSpecialChars

writeStringToFile :: FilePath -> String -> IO ()
writeStringToFile filePath contents = do
  h <- openFile filePath WriteMode
  hSetEncoding h utf8
  hPutStr h contents
  hClose h

writeSetToFile :: FilePath -> Set.Set String -> IO ()
writeSetToFile filePath set = do
  h <- openFile filePath WriteMode
  hSetEncoding h utf8
  hPutStr h (show set)
  hClose h

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
    mapM_ (\(fileName, content) -> writeStringToFile ("resources/out/" ++ fileName) content) fileContentPairs
    writeSetToFile "resources/out/set.txt" wordsSet 
    print "done"

