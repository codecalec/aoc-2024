import System.IO
import Control.Monad
import Data.List

main = do
    let list = []
    handle <- openFile "input" ReadMode
    contents <- hGetContents handle 
    let levels = map convert (lines contents)
    let safeLevels = filter canBeSafe levels
    print safeLevels
    print $ length safeLevels
    hClose handle

type Level = [Int]

convert :: String -> Level
convert xs = map read (words xs)


isSafe :: Level -> Bool
isSafe xs = ((allPositive nums) || (allNegative nums)) && (safeJump nums)
    where
        nums :: [Int]
        nums = map (\(x,y) -> x-y) (zip (take (length xs - 1)  xs) (drop 1 xs))


allPositive :: [Int] -> Bool
allPositive (x:xs) = if (x > 0) then allPositive xs else False
allPositive [] = True

allNegative :: [Int] -> Bool
allNegative (x:xs) = if (x < 0) then allNegative xs else False
allNegative [] = True

safeJump :: [Int] -> Bool
safeJump (x:xs) = if (num >= 1 && num <= 3) then safeJump xs else False
    where num = abs x
safeJump [] = True


canBeSafe :: Level -> Bool
canBeSafe xs = _canBeSafe xs (length xs)

_canBeSafe :: Level -> Int -> Bool
_canBeSafe _ 0 = False
_canBeSafe xs idx = if isSafe dropped then True else _canBeSafe xs (idx-1)
    where
        dropped = (take (idx-1) xs) ++ (drop idx xs)
