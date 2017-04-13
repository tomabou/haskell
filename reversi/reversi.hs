{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE IncoherentInstances #-}
import qualified Data.Map.Strict as Map

data Mark = Black | White | Empty deriving (Eq)
instance Show Mark where 
    show Black = "X"
    show White = "O"
    show Empty = "_"

type Pos = (Int,Int)
type Board = Map.Map Pos Mark

instance Show Board where
    show bd = showFunc (1,1) bd

showFunc :: Pos -> Board -> String
showFunc (a,b) bd 
    |mod a 8 /= 0 = show (bd Map.! (a,b)) ++ showFunc (a+1,b) bd 
    |b == 8       = show $ bd Map.! (8,8)
    |otherwise    = show (bd Map.! (a,b)) ++ "\n" ++ showFunc (1,b+1) bd

emptyBoard = Map.fromList [((x,y),Empty)| x<-[1..8],y<-[1..8]] :: Board 

putStone :: Pos -> Board -> Board
