import Control.Monad
import Data.Ratio
--import Control.Parallel.Strategies

data Equation = Number Integer | Operator String Equation Equation 

instance Show Equation where 
    show (Number x) = show x
    show (Operator ('$':xs) a b) = "(" ++ show b ++ " " ++ xs ++" "++show a ++")"
    show (Operator xs a b) ="("++ (show a) ++" "++ xs++" " ++(show b)++")"

calculate :: Equation -> Maybe Rational
calculate (Number x) = Just(x % 1)
calculate (Operator "+" a b) = (+) <$> calculate a <*> calculate b
calculate (Operator "*" a b) = (*) <$> calculate a <*> calculate b
calculate (Operator "-" a b) = (-) <$> calculate a <*> calculate b
calculate (Operator "$-" a b) = (-) <$> calculate b <*> calculate a
calculate (Operator "/" a b) 
    |calculate b /= Just 0 = (/) <$> calculate a <*> calculate b
    |otherwise = Nothing
calculate (Operator "$/" a b) 
    |calculate a /= Just 0 = (/) <$> calculate b <*> calculate a
    |otherwise = Nothing

makeEq :: [Integer] ->[Equation]
makeEq [] = []
makeEq [x] = [Number x]
makeEq (x:xs) = do
    let n = 1 + length xs
    i <-[0..(n-2)]
    ope <- ["+","*","-","$-","/","$/"]
    (ps,qs)<- combi i xs
    a <- makeEq (x:ps)
    b <- makeEq (qs)
    return (Operator ope a b)

combi :: Int -> [a] -> [([a],[a])]
combi 0 xs = [([],xs)]
combi n (x:xs) 
    |length xs + 1 == n = [(x:xs,[])]
    |otherwise = [(x:ps,qs)|(ps,qs)<-combi (n-1) xs] 
        ++ [(ps,x:qs)|(ps,qs)<- combi n xs]


headMaybe::[a] -> Maybe a
headMaybe [] = Nothing
headMaybe xs = Just (head xs) 

showMaybe ::(Show a)=> Maybe a -> String
showMaybe (Just a) = show a
showMaybe Nothing = "No Answer"
{-}
parMap :: (a->b) -> [a] -> Eval[b]
parMap f [] = return []
parMap f (a:as) = do
    b <- rpar (f a)
    bs <- parMap f as
    return (b:bs)

main = do
    let i = 1000000
    let numbers = [1,2,3,4,5]
    let ans = headMaybe . (filter (\x -> calculate x == Just (i%1))) $ makeEq numbers
    putStrLn (show i ++" = " ++ showMaybe ans)
  -}  

main = forever $ do
    putStrLn "What Integer do you want to make?"
    i <- readLn ::IO Integer
    putStrLn "Input numbers like [1,2,3,4]"
    numbers <- readLn :: IO [Integer]
    let ans = headMaybe . (filter (\x -> calculate x == Just (i%1))) $ makeEq numbers
    putStrLn (show i ++" = " ++ showMaybe ans)
    putStrLn ""
