import System.IO
import Data.List
import Data.Maybe

import qualified Data.Map as Map

main = do
    handle <- openFile "input" ReadMode
    contents <- hGetContents handle 

    let instr = filter (elem '|') (lines contents)
    let m = makeMap $ (map getPair instr)
    print m

    let pages = map getPages $ filter (elem ',') (lines contents)
    print pages

    let unordered = filter (\x -> not $ isOrdered m x) pages
    print $ sum $ map getMiddleValue $ map (orderPages m) unordered

    hClose handle

getMiddleValue :: [String] -> Int
getMiddleValue xs = read $ xs !! (div (length xs)  2)

getPair :: String -> (String, String)
getPair xs = (take idx xs, drop (idx+1) xs)
    where 
        idx = fromJust $ elemIndex '|' xs

getPages :: String -> [String]
getPages [] = []
getPages xs = case elemIndex ',' xs of
    Just idx -> [take idx xs] ++ getPages (drop (idx+1) xs)
    Nothing -> [xs]

isOrdered :: (Map.Map String [String]) -> [String] -> Bool
isOrdered _ [] = True
isOrdered _ [x] = True
isOrdered m (x:xs) = if (elem y values) then isOrdered m xs else False
    where 
        y :: String
        y = xs !! 0

        values :: [String]
        values = case Map.lookup x m of
            Just x -> x
            Nothing -> []

orderPages :: (Map.Map String [String]) -> [String] -> [String]
orderPages m xs = sortBy sort' xs
    where
        sort' :: String -> String -> Ordering 
        sort' x y = if (elem y (safeLookup x m)) then LT else GT

safeLookup :: String -> Map.Map String [String]  -> [String]
safeLookup k m = case Map.lookup k m of
    Just v -> v
    Nothing -> []


makeMap :: [(String, String)] -> Map.Map String [String]
makeMap xs = foldl (\acc (k, v)  -> (Map.adjust (update' v) k acc)) m xs 
    where
        m = foldl (\acc (k,_)  -> (Map.insert k [] acc)) (Map.empty) xs 
        update' v xs = xs ++ [v]

