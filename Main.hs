module Main where 

weirdAlgoSolve :: Integer -> [Integer]
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

main = do
  raw <- getLine 
  let input  = read raw :: Integer
      result = weirdAlgoSolve input 
  putStrLn $ unwords $ map show result

