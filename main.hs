--{-# LANGUAGE BlockArguments #-}
--Nim game

board :: [Int]
board = [1, 3, 5, 7]

{-line1 = 1

line2 = 3

line3 = 5

line4 = 7

board = [line1, line2, line3, line4]-}

gameLoop :: [Int] -> IO ()
gameLoop board = do
  putStrLn "Enter a number"
  number <- getLine
  let lineNumber = read number :: Int
  putStrLn "Enter a quantity"
  quantity2 <- getLine
  let quantity = read quantity2 :: Int
  let lineVal = getLineVal board lineNumber
  if lineVal `elem` board
    then do
      let newLineValue = subtract quantity lineVal
      let newBoard = setLineVal board lineNumber newLineValue
      if checkWin newBoard
        then do
          putStrLn "You win!"
          --return ()
        else do
          gameLoop newBoard
    else do
      putStrLn "Invalid number"
      gameLoop board

{-getNumber :: IO Int
getNumber = do
  --putStrLn "Enter a number"
  number <- getLine
  let n = read number :: Int
  return n
  --return number-}

getLineVal :: [Int] -> Int -> Int
getLineVal board n = board !! n

setLineVal :: [Int] -> Int -> Int -> [Int]
setLineVal board n val =
  let (x, _ : y) = splitAt n board
   in x ++ [val] ++ y

checkWin :: [Int] -> Bool
checkWin board = all (== 0) board