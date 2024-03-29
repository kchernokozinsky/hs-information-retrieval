import Utils.IO
import qualified Data.Set as Set
import IR.IncidenceMatrix as Matrix
import IR.InvertedIndex as Index
import IR.CoordinateInvertedIndex as CoordIndex
import IR.DoubleInvertedIndex as DoubleIndex
import IR.Set as S
import Data.List as List
import IR.Trie as Trie
import IR.KGramIndex as KGram

main :: IO ()
main = do

    -- READ FILES --

    let folder = "resources/fb2"
    fileNames <- getFileNames folder
    contents <- mapM readFileWithEncoding $ map ((folder ++ "/") ++) fileNames
    let fileContentPairs = zip fileNames contents

    -- -- CREATE SET --

    let wordsSet = S.uniqueWords contents
    print $ "unique terms size: " ++ show(List.length wordsSet)
    print $ "collection size: " ++show(List.length $ S.words contents)
    writeListToFile "resources/out/set.txt" wordsSet
    serializeToFile "resources/out/serialized/set.bin" wordsSet

    -- -- CREATE INCIDENCE MATRIX TERM - DOCUMENT --

    let incidenceMatrix = buildIncidenceMatrix fileContentPairs
    writeIncidenceMatrixToFile "resources/out/incidence-matrix.txt" incidenceMatrix
    print "INCIDENCE MATRIX---------------------------------"
    print $ "apple AND veil: " ++ show(Matrix.search incidenceMatrix "apple AND veil")
    print $ "apple OR veil: " ++ show(Matrix.search incidenceMatrix "apple OR veil")
    print $ "NOT veil: " ++ show(Matrix.search incidenceMatrix "NOT veil")
    print $ "apple AND unimpeachable: " ++ show(Matrix.search incidenceMatrix "apple AND unimpeachable")
    print $ "apple OR unimpeachable: " ++ show(Matrix.search incidenceMatrix "apple OR unimpeachable")

    -- CREATE INVERTED INDEX --

    let invertedIndex = buildInvertedIndex fileContentPairs
    writeInvertedIndexToFile "resources/out/inverted-index.txt" invertedIndex
    print "INVERTED INDEX---------------------------------"
    print $ "apple AND veil: " ++ show(Index.search invertedIndex "apple AND veil")
    print $ "apple OR veil: " ++ show(Index.search invertedIndex "apple OR veil")
    print $ "NOT veil: " ++ show(Index.search invertedIndex "NOT veil")
    print $ "apple AND unimpeachable: " ++ show(Index.search invertedIndex "apple AND unimpeachable")
    print $ "apple OR unimpeachable: " ++ show(Index.search invertedIndex "apple OR unimpeachable")

    -- CREATE COORDINATE INVERTED INDEX --

    let coordinateInvertedIndex = buildCoordinateInvertedIndex fileContentPairs
    writeCoordinateInvertedIndexToFile "resources/out/coord-inverted-index.txt" coordinateInvertedIndex
    print "COORDINATE INVERTED INDEX---------------------------------"
    print $ "cowbellied\\{2}Thou: " ++ show(CoordIndex.search coordinateInvertedIndex "cowbellied\\{2}Thou")
    print $ "I\\{2}come\\{1}down\\{3}north: " ++ show(CoordIndex.search coordinateInvertedIndex "cowbellied\\{2}Thou")

    -- CREATE DOUBLE INVERTED INDEX --

    let doubleInvertedIndex = buildDoubleInvertedIndex fileContentPairs
    writeInvertedIndexToFile "resources/out/double-inverted-index.txt" doubleInvertedIndex
    print "DOUBLE INVERTED INDEX---------------------------------"
    print $ "Douglas coming down the stair: " ++ show(DoubleIndex.search doubleInvertedIndex "Douglas coming down the stair")
    print $ "said Holmes and his: " ++ show(DoubleIndex.search doubleInvertedIndex "said Holmes and his")
    print $ "I had come down from the north: " ++ show(DoubleIndex.search doubleInvertedIndex "I had come down from the north")

    -- CREATE TRIE with PERMUTATION INDEX --  
     
    let trie = Trie.build wordsSet
    print "TRIE---------------------------------" 
    print $ "ab*min*: " ++ show(Trie.match' "ab*min*" trie)
    print $ "blu*pett*coate*: " ++ show(Trie.match' "blu*pett*coate*" trie)
    print $ "co*ceiva*: " ++ show(Trie.match' "co*ceiva*" trie)

    -- CREATE KGRAMS INDEX --  
    let kGramIndex = KGram.buildKGramIndex wordsSet
    writeInvertedIndexToFile "resources/out/kgram-index.txt" kGramIndex
    print "KGRAMS INDEX---------------------------------"
    -- print $ "wor*: " ++ show(KGram.search kGramIndex "wor*")
    print $ "ab*min*: " ++ show(KGram.search kGramIndex "ab*min*")
    print $ "blu*pett*coate*: " ++ show(KGram.search kGramIndex "blu*pett*coate*")
    print $ "co*ceiva*: " ++ show(KGram.search kGramIndex "co*ceiva*")
    print "done"
    

