import Control.Monad
import Data.Ratio
import Data.List

solveRPN :: String -> (Maybe Rational,String)
solveRPN st = ((do
    [ans]<-foldM foldingFunc [] (words st)
    return ans),
    st)

foldingFunc :: [Rational] -> String -> Maybe [Rational]
foldingFunc (x:y:xs) "*" = Just ((x*y):xs)
foldingFunc (x:y:xs) "+" = Just((x+y):xs)
foldingFunc (x:y:xs) "-" = Just((y-x):xs)
foldingFunc (x:y:xs) "/" 
    |x == 0    = Nothing
    |otherwise = Just((y/x):xs)
foldingFunc xs numberString = Just ((:xs).(% 1)$read numberString)

perms ::(Eq a)=> [a]-> [[a]]
perms [] = [[]]
perms (x:xs) = nub  $ do
    ys <- perms xs
    map (\(ys,zs)->ys ++ [x] ++ zs) (cuts ys)

cuts :: [a]-> [([a],[a])]
cuts []= [([],[])]
cuts (x:xs) = [([],(x:xs))]++ map (\(ys,zs)->([x]++ys,zs)) (cuts xs)

makeShiki :: (Show a) => [a] -> [String]
makeShiki xs = do
    ctln <- catalan (length xs - 1)
    foldM foldMakeShiki [] (zipCtlnNum ([1]++ctln) xs)

zipCtlnNum::[Int]->[a]->[Maybe a]
zipCtlnNum [] _ = []
zipCtlnNum (x:xs) as 
    |x == 1    = (Just (head as)):zipCtlnNum xs (tail as)
    |otherwise = Nothing:zipCtlnNum xs as

foldMakeShiki :: (Show a) => String -> Maybe a -> [String]
foldMakeShiki str Nothing = [str ++ ope | ope <- ["+ ","* ","- ","/ "]]
foldMakeShiki str (Just a) = [str ++ show a ++" "]

catalan :: Int -> [[Int]]
catalan 0 = [[]]
catalan n = foldl (foldCatalan n) [] [1..n]

foldCatalan :: Int ->[[Int]] -> Int -> [[Int]]
foldCatalan n xs i = (++) xs $ do
    ys <- catalan (i-1)
    zs <- catalan (n-i)
    return ([1]++ys ++[0]++zs)

headMaybe::[a] -> Maybe a
headMaybe [] = Nothing
headMaybe xs = Just (head xs) 

main = forever $ do
    putStrLn "What Integer do you want to make?"
    i <- readLn ::IO Integer
    putStrLn "Write numbers you should use like [1,2,3,4]"
    numbers <- readLn :: IO [Int]
    let ans = show.headMaybe.filter (\(x,_) ->(x == Just (i%1))) .map solveRPN $ makeShiki =<< perms (numbers)
    putStrLn ans
    putStrLn ""

