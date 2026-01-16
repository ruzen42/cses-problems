module Lib
    ( weirdAlgoSolve
    , weirdAlgoSolve'
    , weirdAlgoSolve''
    , distinctNumsSolve
    , twoKnightsSolve
    ) where

import Data.List (sort, group) 

weirdAlgoSolve :: Int -> [Int]
weirdAlgoSolve input = 
  if odd input then
    if input == 1 then 
      [input] 
    else
      let arr = weirdAlgoSolve $ input * 3 + 1
      in input:arr
  else 
    let arr = weirdAlgoSolve $ input `div`  2
    in input:arr

weirdAlgoSolve' :: Int -> [Int]
weirdAlgoSolve' n = takeWhileInclusive (/= 1) $ iterate next n
  where next x | even x = x `div` 2
               | otherwise = x * 3 + 1
        takeWhileInclusive :: (a -> Bool) -> [a] -> [a]
        takeWhileInclusive _ [] = []
        takeWhileInclusive p (x:xs) = x : if p x then takeWhileInclusive p xs else []

weirdAlgoSolve'' :: Int -> [Int]
weirdAlgoSolve'' n = go n [] 
  where go 1 acc = reverse (1 : acc)
        go x acc | even x = go (x `div` 2) (x : acc)
                 | otherwise = go (x * 3 + 1) (x : acc)

distinctNumsSolve :: [Int] -> Int 
distinctNumsSolve arr = length $ group $ sort arr 
      
twoKnightsSolve :: Int -> [Int]
twoKnightsSolve k = map solve [1..k]
  where 
    solve :: Int -> Int 
    solve k = (k^4 - 9 * k^2 + 24 * k - 16) `div` 2

missingCoinSumSolve :: [Int] -> Int 
missingCoinSumSolve arr =  
  let sorted = sort arr
      solve currrentTarget [] = currrentTarget 
      solve currrentTarget (x:xs)
        | x > currrentTarget = currrentTarget
        | otherwise          = solve (currrentTarget + x) xs
  in solve 1 sorted



