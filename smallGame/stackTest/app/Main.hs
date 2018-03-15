 module Main where

import Math.Number.Theory.Powers.Cubes
import Lib
main = do 
    n <- readLn :: IO Integer
    let lvalus = [(2^a + 3^b + 5,a,b) | a<-[1..n], b<-[1..n]]
    print $ filter isCubic lvalus


