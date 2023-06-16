module IR.TermDocument where
import qualified Data.Set as Set
import Text.Regex.TDFA ((=~), getAllTextMatches, AllTextMatches)
import qualified Data.HashMap.Strict as Map

type Term = String

type DoubleTerm = String

type Document = String

type Coord = Int

type Distance = Int

getWords :: String -> [Term]
getWords str = getAllTextMatches (str =~ "[a-zA-Z0-9']+" :: AllTextMatches [] String)

documentTerms :: String -> [Term]
documentTerms content = Set.toList $ Set.fromList (getWords content)


        
documentTerms' :: String -> [(Term, [Coord])]
documentTerms' content = Map.toList $ foldl(\acc (term, coord) -> Map.insertWith (++) term [coord] acc) (Map.empty :: Map.HashMap Term [Coord]) termCoord
    where termCoord = (zip (getWords content) [1..])

documentTerms'' :: String -> [DoubleTerm]
documentTerms'' content = getDoubleTerms $ getWords content
    where getDoubleTerms :: [Term] -> [DoubleTerm]
          getDoubleTerms terms = zipWith (\t1 t2 -> t1 ++ " " ++ t2) terms (tail terms)