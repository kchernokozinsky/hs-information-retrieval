module IR.PermutationIndex where
import Data.List.Split
import Text.Regex

permutationIndex :: String -> [String]
permutationIndex str = p' (str ++ "$")  []
    where
        p' :: String -> [String] -> [String]
        p' str acc = if (str !! 0) == '$' then acc ++ [str]
                        else p' (tail str ++ [head str]) (acc ++ [str])

processQuery :: String -> String
processQuery str = p' (str ++ "$")
    where
        p' :: String -> String
        p' str = if (last str) == '*' then str
                                      else p' (tail str ++ [head str])
        split = splitOn "*" str
        processed = subRegex (mkRegex "\\*.*\\*") str "*"
    
processQuery' :: String -> (String, [String])
processQuery' str = (p' (processed ++ "$"), split)
    where
        p' :: String -> String
        p' str = if (last str) == '*' then str
                                      else p' (tail str ++ [head str])
        split = splitOn "*" str
        processed = subRegex (mkRegex "\\*.*\\*") str "*"