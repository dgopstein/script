--{-# LANGUAGE OverloadedStrings #-}
-- http://svn.code.sf.net/p/cmusphinx/code/trunk/cmudict/
--import qualified Data.Text as T
--main = print $ T.splitOn " " "this is a test"
--main = putStrLn $ (readFile "cmudict.0.7a" >>= (T.splitOn (T.pack "\n") . T.pack)) >>= head

--split :: Char -> String -> [String]
--split token source = let (word, res) = partition (token ==) source in
    

--main = print $ split "," "a, sentence, split by, commas"

--main = do
--    file <- readFile "cmudict.0.7a"
--    putStrLn $ T.splitOn (T.pack "\n") (T.pack file)
--    return []
import Data.Maybe
import Data.List
import Data.Char
import qualified Data.Map as Map

parseLine :: [Char] -> Maybe (String, Int)
parseLine (';' : _) = Nothing
parseLine line = Just (word, (count isNumeric rest)) where
    (word, rest) = break (' ' ==) line

readDict :: IO ([(String, Int)])
--readDict = readFile "cmudict.0.7a" >>= (return catMaybes) . (map parseLine) . lines
readDict = do
    file <- readFile "cmudict.0.7a"
    return $ (catMaybes . (map parseLine) . lines) file

count :: (a -> Bool) -> [a] -> Int
count cond = length . (filter cond)

countEquality :: Eq a => a -> [a] -> Int
countEquality x = count (x ==)

isNumeric :: Char -> Bool
isNumeric char = char >= '0' && char <= '9'

syllableMap = Map.fromDistinctAscList 

countSyllables smap word = Map.findWithDefault 0 (map toUpper word) smap

main = do
    dict <- readDict
    print $ countSyllables (syllableMap dict) "haiku"
    return []
