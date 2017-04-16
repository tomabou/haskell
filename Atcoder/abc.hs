main = do
    a <- getLine
    b <- getLine
    putStrLn $ func a b
func xs [] = xs
func (x:xs) (y:ys) = x:y:func xs ys