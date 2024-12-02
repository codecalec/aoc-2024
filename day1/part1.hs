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

    let totalDiff = sum (map (\(x,y) -> (abs (x - y))) (zip leftList rightList))
    print totalDiff

    hClose handle


f :: [String] -> [Int]
f = map read

first :: Num a => [a] -> [a]
first (x:y:xs) = x : first xs
first _ = []

second :: Num a => [a] -> [a]
second (x:y:xs) = y : second xs
second _ = []


