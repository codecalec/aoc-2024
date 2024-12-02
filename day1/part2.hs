import System.IO
import Control.Monad
import Data.List

main = do
    let list = []
    handle <- openFile "input" ReadMode
    contents <- hGetContents handle
    let singlewords = words contents
        list = f singlewords
    let leftList = sort $ first list
    let rightList = sort $ second list

    let totalOccurences =  sum $ map (\x -> x * (occurences rightList x)) leftList
    print totalOccurences

    hClose handle


f :: [String] -> [Int]
f = map read

first :: Num a => [a] -> [a]
first (x:y:xs) = x : first xs
first _ = []

second :: Num a => [a] -> [a]
second (x:y:xs) = y : second xs
second _ = []

occurences :: Eq a => [a] -> a -> Int
occurences xs y = occurences_ xs y 0

occurences_ :: Eq a => [a] -> a -> Int -> Int
occurences_ (x:xs) y count = 
    if (x == y) then
        (occurences_ xs y count+1)
    else 
        (occurences_ xs y count)

occurences_ _ _ count = count
