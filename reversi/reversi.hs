{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE IncoherentInstances #-}
import Data.Array.IArray 
import Control.Monad
import Data.List
import Data.Function

data Mark = Black | White | Empty deriving (Eq)
instance Show Mark where 
    show Black = "X"
    show White = "O"
    show Empty = "_"

oppo :: Mark -> Mark 
oppo Black = White 
oppo White = Black

type Pos = (Int,Int)

(<+>) :: Pos -> Pos -> Pos
(<+>) (a,b) (c,d) = (a+c,b+d)
type Board = Array Pos Mark

data RevState = RevState {board :: Board , turn::Mark , preMove :: Pos} deriving (Show)

instance Show Board where
    show bd = "\n12345678\n" ++ showFunc (1,1) bd

showFunc :: Pos -> Board -> String
showFunc (a,b) bd 
    |mod a 8 /= 0 = show (bd ! (a,b)) ++ showFunc (a+1,b) bd 
    |b == 8       = (show $ bd ! (8,8) )++ "8"
    |otherwise    = show (bd ! (a,b)) ++ show b ++ "\n" ++ showFunc (1,b+1) bd

emptyBoard = array ((1,1),(8,8)) [((x,y),Empty)| x<-[1..8],y<-[1..8]] :: Board 
startBoard = emptyBoard // [((4,5),Black), ((5,4),Black),((4,4),White),((5,5),White)]
startState = RevState startBoard Black (4,4) 

eightDir = filter (/=(0,0)) [(x,y) | x<-[(-1),1,0], y<-[(-1),1,0]]

isLegal :: RevState -> Pos -> Bool
isLegal rs ps 
    | not $ isOnBoard ps = False
    | bd ! ps /= Empty = False
    | otherwise = any (canReverse bd ps (turn rs)) eightDir
    where bd = board rs

canReverse :: Board -> Pos -> Mark -> (Int,Int) -> Bool
canReverse bd ps trn dir 
    | not $ isOnBoard (ps<+>dir) = False
    | bd ! (ps <+> dir) == Empty = False
    | bd ! (ps <+> dir) == trn   = False
    | otherwise = thereIsSame bd ps trn dir

thereIsSame :: Board -> Pos -> Mark -> (Int,Int) -> Bool
thereIsSame bd ps trn dir 
    | not $ isOnBoard (ps<+>dir) = False 
    | bd !(ps<+>dir) == Empty = False
    | bd !(ps<+>dir) == trn = True
    | otherwise = thereIsSame bd (ps<+>dir) trn dir

isOnBoard :: Pos -> Bool
isOnBoard (a,b) = 0<a && a<9 && 0<b && b<9


putStone :: Pos -> RevState -> RevState
putStone ps rs = RevState (board rs// changeList) (oppo $ turn rs) ps
    where changeList = (ps,(turn rs)) :  ( join $ map (ptStFunc (board rs) ps (turn rs)) eightDir ) 

ptStFunc :: Board ->Pos -> Mark -> (Int ,Int) -> [(Pos,Mark)]
ptStFunc bd ps trn dir  
    | canReverse bd ps trn dir = (ps<+>dir,trn) : untilSame bd (ps<+>dir) trn dir
    | otherwise  = []

untilSame :: Board -> Pos -> Mark -> (Int, Int) -> [(Pos,Mark)] 
untilSame bd ps trn dir  
    | bd ! (ps<+>dir) == trn = []
    | otherwise = (ps<+>dir,trn) : untilSame bd (ps<+>dir) trn dir

moves :: RevState -> [RevState]
moves rs = do
    mv <- filter (isLegal rs) [(x,y) | x<- [1..8], y<-[1..8]]
    return (putStone mv rs) 

--eval represents the advantage of the turn player
evalGameTree ::(Tree RevState) -> Int
evalGameTree (Node rs []) = evalRevState rs
evalGameTree (Node _ xs) = - (minimum  (map evalGameTree xs) )

evalRevState :: RevState -> Int
evalRevState rs = length . filter (isLegal rs) $ [(x,y)| x<-[1..8], y <- [1..8]]

data Tree a = Node a [Tree a] deriving　(Show)

repTree :: (a-> [a]) -> a -> Tree a
repTree f a = Node a (map (repTree f) (f a))

func:: Int -> [Int]
func 0 = []
func n = [(n-1),(n-1)]

prune :: Int -> Tree a-> Tree a
prune 0 (Node a _) = Node a []
prune n (Node a xs) = Node a (map (prune $ n-1) xs )

reversiAI :: RevState -> RevState 
reversiAI rs = maximumBy (compare `on` evalGameTree . (prune 3) . (repTree moves) ) (moves rs)


main = do 
    putStrLn "Lets play Reversi!!"
    let rs = startState
    print $ board rs
    nextMove rs

nextMove :: RevState -> IO ()
nextMove rs = do
    putStrLn "enter next Move as 3 4"
    str <- getLine::IO String 
    if elem str [show x ++ " " ++ show y | x<-[1..8] , y<-[1..8]] then return () else nextMove rs
    let (a:b:_) = (map read) . words $ str
    let ps = (a,b)
    if isLegal rs ps 
        then do 
            let nextRs = putStone ps rs
            print $ board nextRs
            putStrLn "AI thinking now"
            let nNextRs = reversiAI nextRs
            print $ board nNextRs
            nextMove nNextRs
        else do 
            putStrLn "illegal move"
            nextMove rs
    