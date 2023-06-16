module IR.Set where
import qualified Data.Set as S
import Text.Regex.TDFA ((=~), getAllTextMatches, AllTextMatches)
import Data.Binary (Binary, encode, decode)
import qualified Data.ByteString.Lazy as B
import System.IO

getWords :: String -> [String]
getWords str = getAllTextMatches (str =~ "[a-zA-Z0-9']+" :: AllTextMatches [] String)

uniqueWords :: [String] -> [String]
uniqueWords contents = S.toList $ foldl (\acc content -> acc `S.union` S.fromList (getWords content)) S.empty contents

serializeToFile :: Binary a => FilePath -> [a] -> IO ()
serializeToFile filePath xs = B.writeFile filePath (encode xs)

deserializeFromFile :: Binary a => FilePath -> IO [a]
deserializeFromFile filePath = do
  contents <- B.readFile filePath
  return (decode contents)