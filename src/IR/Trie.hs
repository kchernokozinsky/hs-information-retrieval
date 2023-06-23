module IR.Trie where
import qualified Data.HashMap.Strict as HM
import qualified IR.PermutationIndex as PI
import Data.List (isInfixOf)
data Trie = Root (HM.HashMap Char Trie) | Node Char Bool Value (HM.HashMap Char Trie) deriving (Show)

data Value = Value String | Empty deriving (Show)

fromValue :: Value -> String
fromValue (Value str) = str

empty :: Trie
empty = Root HM.empty

build :: [String] -> Trie
build strs = foldl (\acc str -> putPermIndex str acc) empty strs

putPermIndex :: String -> Trie -> Trie
putPermIndex str trie = foldl (\acc p -> put p str acc) trie perms
    where perms = PI.permutationIndex str

put' :: String -> Trie -> Trie
put' str trie = put str str trie

put :: String -> String -> Trie -> Trie
put [] _ trie = trie
put (x:[]) value (Root ts) = Root (put' x value ts)
 where put' x value ts = if x `HM.member` ts 
                   then case (x `HM.lookup` ts) of
                     Just (Node c b v ts) -> HM.insert x (Node c True (Value value) ts) ts
                     Nothing -> error "This should never happen"
                   else HM.insert x (Node x True (Value value) HM.empty) ts
put (x:[]) value (Node c b v ts) = Node c b v (put' x value ts)
  where put' x value ts = if x `HM.member` ts 
                   then case (x `HM.lookup` ts) of
                     Just (Node c b v ts) -> HM.insert x (Node c True (Value value) ts) ts
                     Nothing -> error "This should never happen"
                   else HM.insert x (Node x True (Value value) HM.empty) ts
put (x:xs) value (Node c b v ts) = Node c b v (put' x value ts)
    where put' x value ts = if x `HM.member` ts 
                     then case (x `HM.lookup` ts) of
                         Just t -> HM.insert x (put xs value t) ts
                         Nothing -> error "This should never happen"
                     else HM.insert x (put xs value (Node x False Empty HM.empty)) ts
put (x:xs) value (Root ts) = Root (put' x value ts)
    where put' x value ts  = if x `HM.member` ts 
                     then case (x `HM.lookup` ts) of
                         Just t -> HM.insert x (put xs value t) ts
                         Nothing -> error "This should never happen"
                     else HM.insert x (put xs value (Node x False Empty HM.empty)) ts



search :: String -> Trie -> Bool
search [] (Root ts) = False
search [] (Node c b v ts) = b
search (x:[]) (Root ts) = search' x ts
    where search' x ts = if x `HM.member` ts 
                         then case (x `HM.lookup` ts) of
                             Just (Node _ b _ _) -> b
                             Nothing -> error "This should never happen"
                         else False
search (x:[]) (Node c b v ts) = search' x ts
    where search' x ts = if x `HM.member` ts 
                         then case (x `HM.lookup` ts) of
                             Just (Node _ b _ _) -> b
                             Nothing -> error "This should never happen"
                         else False
search (x:xs) (Node c b v ts) = search' x ts
    where search' x ts = if x `HM.member` ts 
                         then case (x `HM.lookup` ts) of
                             Just t -> search xs t
                             Nothing -> error "This should never happen"
                         else False
search (x:xs) (Root ts) = search' x ts
    where search' x ts = if x `HM.member` ts 
                         then case (x `HM.lookup` ts) of
                             Just t -> search xs t
                             Nothing -> error "This should never happen"
                         else False

                
match' :: String -> Trie -> [String]
match' query trie = filtered $ go (fst query') trie []
    where all (Node c b v ts) acc = case b of 
                                                True  -> [fromValue v] ++ (foldl (\acc t -> acc ++ (all (ts HM.! t) []) ) acc (HM.keys ts))
                                                False ->(foldl (\acc t -> acc ++ (all (ts HM.! t) []) ) acc (HM.keys ts))
          all (Root ts) acc = foldl (\acc t -> acc ++ (all (ts HM.! t) []) ) acc (HM.keys ts)
          go :: String -> Trie -> [String] -> [String]
          go [] (Root ts) acc = acc
          go [] (Node c b v ts) acc = if b then acc ++ [fromValue v] else acc
          go ('*':[]) trie acc = all trie acc
          go (x:xs) (Root ts) acc = if x `HM.member` ts then go xs (ts HM.! x) acc
                                                            else acc
          go (x:xs) (Node c b v ts) acc = if x `HM.member` ts then go xs (ts HM.! x) acc
                                                                else acc
          query' = PI.processQuery' query

          filtered str = filter (\x -> foldl(\acc part -> acc && (part `isInfixOf` x)) True (snd query') ) str 
        