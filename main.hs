--Nim game

import Control.Monad

board :: [Int]
board = [1, 3, 5, 7]

printWhoIsPlaying :: Bool -> IO ()
printWhoIsPlaying p = do
  if p
    then putStrLn "Human Player is playing"
    else putStrLn "Computer is playing"

--changeTurn :: t0 -> Bool -> Bool
--changeTurn t = return not t

selectDifficulty :: IO Int
selectDifficulty = do
  putStrLn "Select difficulty: "
  putStrLn "1. Easy"
  putStrLn "2. Hard"
  putStrLn "0. Exit"
  putStr "> "
  difficulty <- getLine
  let d = (read difficulty :: Int)
  if d == 0
    then return 99
    else return d

{-gameMenu :: IO ()
gameMenu = do --TODO fix the menu
  putStrLn "Welcome to Nim!"
  putStrLn "Are you ready?"
  putStrLn "1. Yes, play"
  putStrLn "0. No, exit"
  putStr "> "
  option <- getLine
  if (read option :: Int) == 1
    then --let level = selectDifficulty -- TODO: implement difficulty selection

      let level = 1
       in if level == 1
            then isPlayerPlaying True --TODO fix this
            else
              if level == 2
                then isPlayerPlaying False
                else putStrLn "Invalid option"
    else putStrLn "Bye!"-}

printBoard :: [Int] -> IO () --TODO print the complete line
printBoard b = do
  Control.Monad.when (not (null b)) $ do
    let line = "|" ++ (show (head b)) ++ "|"
    putStrLn line
    printBoard (tail b)

gameLoop :: [Int] -> Bool -> IO ()
gameLoop board player = do
  putStrLn "Board:"
  printBoard board
  printWhoIsPlaying player
  putStrLn "Enter the line number"
  putStr "> "
  number <- getLine
  let lineNumber = read number :: Int
  putStrLn "Enter a how much sticks to take"
  putStr "> "
  quantity2 <- getLine
  let quantity = read quantity2 :: Int
  if quantity <= 0 --checks if the quantity is valid
    then do
      putStrLn "Invalid quantity, please select at least 1 stick on each turn"
      gameLoop board player
    else do
      let lineVal = getLineVal board lineNumber
      if lineNumber >= 0 && lineNumber < 4 --checks if the line exists
        then do
          if lineVal >= quantity --checks if the stickers exist
            then do
              let newLineValue = subtract quantity lineVal
              let newBoard = setLineVal board lineNumber newLineValue
              if checkWin newBoard
                then do
                  putStrLn "Board:"
                  printBoard newBoard
                  if player then putStrLn "You win!" else putStrLn "Computer wins!"
                else do
                  gameLoop newBoard (not player)
            else do
              putStrLn "Invalid move, quantity entered is greater than the number of sticks in this line"
              gameLoop board player
        else do
          putStrLn "Invalid line number, please select a number between 0 and 3"
          gameLoop board player

--Board functions
getLineVal :: [Int] -> Int -> Int
getLineVal board n = board !! n

setLineVal :: [Int] -> Int -> Int -> [Int]
setLineVal board n val =
  let (x, _ : y) = splitAt n board
   in x ++ [val] ++ y

checkWin :: [Int] -> Bool
checkWin board = all (== 0) board