-- http://svn.code.sf.net/p/cmusphinx/code/trunk/cmudict/

import Control.Applicative
import Data.Maybe
import Data.List
import Data.List.Split
import Data.Char
import qualified Data.Map as Map
import System.Environment
import System.IO

type SylMap = Map.Map String Int

count :: (a -> Bool) -> [a] -> Int
count cond = length . (filter cond)

-------------------------
-- Read the CMU dict file
-------------------------

scrubWord :: String -> String
scrubWord = (map toUpper) . (filter isAlpha)

parseLine :: [Char] -> Maybe (String, Int)
parseLine (';' : _) = Nothing
parseLine line = if (isInfixOf "(" word) then Nothing
                 else Just ((scrubWord word), (count isDigit rest))
                    where (word, rest) = break (' ' ==) line

readDict :: IO ([(String, Int)])
readDict = catMaybes . (map parseLine) . lines <$> readFile "cmudict.0.7a" 

readSylMap :: IO (Map.Map String Int)
readSylMap = fmap Map.fromDistinctAscList readDict 

countWordSyllables :: SylMap -> String -> Int
countWordSyllables sylMap word = Map.findWithDefault 0 (scrubWord word) sylMap

-----------------------
-- Process user input
-----------------------

wordsFromLine :: String -> [String]
wordsFromLine line = (splitOn " ") $ line

countLineSyllables :: SylMap -> String -> Int
countLineSyllables sylMap line = sum $ map (countWordSyllables sylMap) $ wordsFromLine line

--countSyllables :: SylMap -> String -> Int
--countSyllables sylMap block = sum $ map (countLineSyllables sylMap) $ (splitOn "\n") block

-------------------
-- Format output
-------------------

formatCount :: (String -> Int) -> String -> String -> String
formatCount counter delim string = show (counter string) ++ delim

prependWordSyllables :: SylMap -> String -> String
prependWordSyllables sylMap word = (formatCount (countWordSyllables sylMap) "|" word) ++ word

prependLineSyllables :: SylMap -> String -> String
prependLineSyllables sylMap "" = ""
prependLineSyllables sylMap line = (formatCount (countLineSyllables sylMap) " " line) ++
                                    (intercalate " " $ map (prependWordSyllables sylMap) (wordsFromLine line))

stdinMain = do
    sylMap <- readSylMap
    print "Enter the haiku: "
    line   <- getLine
    print $ countLineSyllables sylMap line
    return []

fileMain = do
    args <- getArgs
    let haikuFile = head args
    sylMap <- readSylMap
    haiku <- lines <$> readFile haikuFile
    let prefixedLines = map (prependLineSyllables sylMap) haiku
    putStr $ intercalate "\n" prefixedLines ++ "\n"
    return "" 

main =  fileMain
