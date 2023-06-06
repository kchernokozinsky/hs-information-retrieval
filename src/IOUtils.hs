module IOUtils where 

import System.Directory
import System.IO
import Text.Regex
import qualified Data.Text as T
import qualified Data.Set as Set

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