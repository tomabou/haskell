import Data.List
import Data.Function
import Control.Monad
main = do
    (n,m) <- inputPair
    as <- replicateM n inputPair
    cs <- replicateM m inputPair
    mapM print $ map (solve cs) as

inputPair :: IO (Int,Int)
inputPair = (\[x,y] -> (x,y)).(map read).words <$> getLine

manDist :: (Int ,Int) -> (Int,Int) -> Int
manDist (a,b) (c,d) = abs (a-c) + abs (b-d)

solve :: [(Int,Int)] -> (Int,Int) -> Int
solve xs a=fst $ minimumBy (compare `on` (manDist a).snd) (zip [1..] xs)