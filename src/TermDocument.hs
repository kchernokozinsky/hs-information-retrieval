module TermDocument where
import qualified Data.Set as Set
import Text.Regex.TDFA ((=~), getAllTextMatches, AllTextMatches)

type Term = String
type Document = String

documentTerms :: String -> [String]
documentTerms content = Set.toList $ Set.fromList (getWords content)
    where getWords :: String -> [String]
          getWords str = getAllTextMatches (str =~ "[a-zA-Z0-9']+" :: AllTextMatches [] String)