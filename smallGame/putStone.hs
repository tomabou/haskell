import Data.List
type GameStatus = [Int]
data Player = First | Second deriving (Show, Eq)
winner :: GameStatus -> Player
winner [] = Second
winner [1] = First
winner [2] = First
winner [3] = First
winner xs = 
    let nexts = do
        x <- nub xs
        let ys = delete x xs
        map (++ ys) (cut x)
    in if any (==Second) (map winner nexts) then First else Second

cut::Int -> [[Int]]
cut 1 = [[]]
cut 2 = [[]]
cut 3 = [[],[1]]
cut n = [[n-3],[n-2]] ++ do x <- [1..(n-4)] ; return [x,(n-3-x)]