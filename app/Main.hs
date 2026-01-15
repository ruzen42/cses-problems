module Main (main) where

import Lib
import Control.Parallel.Strategies

void :: a -> IO ()
void _ = pure ()

main :: IO ()
main = do  
  input <- getLine
  let strArr = words input 
      intArr    = map read strArr :: [Int]  
      arr       = [intArr !! 0..intArr !! 1]
      solves = map weirdAlgoSolve'' arr `using` parListChunk 500 rdeepseq
  print solves 
  
