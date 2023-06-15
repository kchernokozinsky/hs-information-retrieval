module IOUtils where 

import System.Directory
import System.IO
import Text.Regex
import qualified Data.Text as T
import qualified Data.Set as Set
import IncidenceMatrix
import InvertedIndex

writeStringToFile :: FilePath -> String -> IO ()
writeStringToFile filePath contents = do
  h <- openFile filePath WriteMode
  hSetEncoding h utf8
  hPutStr h contents
  hClose h

-- Write IncidenceMatrix to file
writeIncidenceMatrixToFile :: FilePath -> IncidenceMatrix -> IO ()
writeIncidenceMatrixToFile path matrix = do
    let lines = incidenceMatrixToStrings matrix
    writeFile path $ unlines lines

-- Write InvertedIndex to file
writeInvertedIndexToFile :: FilePath -> InvertedIndex -> IO ()
writeInvertedIndexToFile filePath index = do
    let indexStrs = invertedIndexToStrings index
    writeFile filePath (unlines indexStrs)

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
        apostrophe = subRegex (mkRegex "&#39;") noTags "'"
        noSpecialChars = subRegex (mkRegex "[^a-zA-Z0-9 ,.;:'`\"!?]") apostrophe "" :: String
        cs = subRegex (mkRegex " +") noSpecialChars " " 
    in trim cs