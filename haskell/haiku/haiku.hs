-- http://svn.code.sf.net/p/cmusphinx/code/trunk/cmudict/

import Data.Maybe
import Data.List
import Data.List.Split
import Data.Char
import qualified Data.Map as Map
import System.IO

type SylMap = Map.Map String Int

parseLine :: [Char] -> Maybe (String, Int)
parseLine (';' : _) = Nothing
parseLine line = Just (word, (count isIntegral rest)) where
    (word, rest) = break (' ' ==) line

readDict :: IO ([(String, Int)])
readDict = readFile "cmudict.0.7a" >>= return . catMaybes . (map parseLine) . lines

readSylMap :: IO (SylMap)
readSylMap = fmap Map.fromDistinctAscList readDict 

count :: (a -> Bool) -> [a] -> Int
count cond = length . (filter cond)

isIntegral :: Char -> Bool
isIntegral char = char >= '0' && char <= '9'

countWordSyllables :: SylMap -> String -> Int
countWordSyllables sylMap word = Map.findWithDefault 0 (map toUpper word) sylMap

wordsFromLine :: String -> [String]
wordsFromLine line = (splitOn " ") $ filter (\x -> isAlpha x || isSpace x) line

countSyllables :: SylMap -> String -> Int
countSyllables sylMap line = sum $ map (countWordSyllables sylMap) $ wordsFromLine line

main = do
    sylMap <- readSylMap
    print "Enter the haiku: \n"
    line   <- getLine
    print $ countSyllables sylMap line
    return []
