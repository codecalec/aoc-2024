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

    let ordered = filter (isOrdered m) pages
    print ordered
    let total = sum $ map getMiddleValue ordered
    print total

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


makeMap :: [(String, String)] -> Map.Map String [String]
makeMap xs = foldl (\acc (k, v)  -> (Map.adjust (update' v) k acc)) m xs 
    where
        m = foldl (\acc (k,_)  -> (Map.insert k [] acc)) (Map.empty) xs 
        update' v xs = xs ++ [v]


-- getOrder :: Map.Map String [String] -> Map.Map String Int
-- getOrder oldmap = Map.foldlWithKey func Map.empty oldmap
--     where
--         func :: (Map.Map String Int) -> String -> [String] -> (Map.Map String Int)
--         func acc k v = Map.insert k (length v) acc
        
