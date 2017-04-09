import Data.List
import Control.Monad.State
import System.Random
import Control.Monad
type Sudoku = ([[Int]] , [[String]])

defaltSudoku = (replicate 9 $ replicate 9 0, replicate 9 $ replicate 9 "123456789") :: Sudoku

isFinish ::Sudoku -> Bool
isFinish (xs,_) = not . (elem 0) . join $ xs

randomRSt (x,y)= state (randomR (x,y))

geneSudoku = do
    a <- randomRSt (0,8)
    b <- randomRSt (0,8)
    

geneSudoku :: State StdGen Sudoku
putNum ::(Int,Int) -> Sudoku -> State StdGen Sudoku
putNum (a,b) (b1,p1) = do
    let numStr = p1 !! a !! b
    n <- randomRSt (0,length numStr - 1)
    let newNum = read [numStr !! n]::Int
    let b2 = changeOne (a,b) newNum b1

    let func p k = 
            (applyOne (k,b) (delete $ numStr!!n))
            .(applyOne (a,k) (delete $ numStr !!n))
            .(applyOne (3* div a 3 + mod k 3,3 *div b 3 + div k 3) (delete $ numStr !!n) )$ p
    let p2 = foldl func p1 [0..8]
    return (b2,p2)

applyOne :: (Int ,Int ) -> (a-> a) -> [[a]] -> [[a]]
applyOne (a,b) f xss= 
    let (yss,(ps:zss)) = splitAt a xss
        (ys,(p:zs)) = splitAt b ps
    in yss ++ [ys ++ [f p] ++ zs] ++ zss

changeOne :: (Int,Int) -> a -> [[a]] -> [[a]]
changeOne (a,b) x xss= 
    let (yss,(ps:zss)) = splitAt a xss
        (ys,(p:zs)) = splitAt b ps
    in yss ++ [ys ++ [x] ++ zs] ++ zss