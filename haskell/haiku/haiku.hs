-- http://svn.code.sf.net/p/cmusphinx/code/trunk/cmudict/

import Data.Maybe
import Data.List
import Data.Char
import qualified Data.Map as Map
import System.IO

parseLine :: [Char] -> Maybe (String, Int)
parseLine (';' : _) = Nothing
parseLine line = Just (word, (count isIntegral rest)) where
    (word, rest) = break (' ' ==) line

readDict :: IO ([(String, Int)])
readDict = readFile "cmudict.0.7a" >>= return . catMaybes . (map parseLine) . lines

readSylMap :: IO (Map.Map String Int)
readSylMap = readDict >>= return . Map.fromDistinctAscList

count :: (a -> Bool) -> [a] -> Int
count cond = length . (filter cond)

isIntegral :: Char -> Bool
isIntegral char = char >= '0' && char <= '9'

countSyllables :: Map.Map String Int -> String -> Int
countSyllables sylMap word = Map.findWithDefault 0 (map toUpper word) sylMap

main = do
    sylMap <- readSylMap
    word   <- getLine
    print $ countSyllables sylMap word
    return []
