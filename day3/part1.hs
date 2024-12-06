import System.IO
import Control.Monad
import Data.List

main = do
    let list = []
    handle <- openFile "input" ReadMode
    contents <- hGetContents handle 
    print $ process contents
    hClose handle

findString :: (Eq a) => [a] -> [a] -> Maybe Int
findString search str = findIndex (isPrefixOf search) (tails str)

process xs = case findString "mul" xs of
    Just idx -> (take endIdx xs) ++ process (drop endIdx xs)
    Nothing -> 0
    where endIdx = findIndex ')'
