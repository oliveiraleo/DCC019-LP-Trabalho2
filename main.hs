--Nim game

import Control.Monad --used in printBoard

board :: [Int]
board = [1, 3, 5, 7]

{-Utilities-}
--getRandomInt :: Int -> Int -> IO Int
--getRandomInt x y = getStdRandom (randomR (x,y))

printWhoIsPlaying :: Bool -> IO ()
printWhoIsPlaying p = do
  if p
    then putStrLn "Human Player is playing"
    else putStrLn "Computer is playing"

{-Game Logic-}
--difficulty level selection
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

--game main menu
gameMenu :: IO ()
gameMenu = do
  putStrLn "Welcome to Nim!"
  putStrLn "Are you ready?"
  putStrLn "1. Yes, play"
  putStrLn "0. No, exit"
  putStr "> "
  option <- getLine
  if (read option :: Int) == 1
    then do
      level <- selectDifficulty
      if level == 1
        then gameLoop board True
        else
          if level == 2
            then gameLoop board False
            else putStrLn "Invalid option"
      gameLoop board True
    else putStrLn "Bye!"

--implements the computer's move
computerTurn :: [Int] -> Bool -> IO [Int]
computerTurn board godMode = do
  if godMode
    then do
      easyComputerTurn board 0 1 --makes a move taking one sticker at a time, starting from the first line
    else do
      putStrLn "Not ready yet" --TODO: implement the computer turn on hard mode
      return board

--implements the easy computer move
easyComputerTurn :: [Int] -> Int -> Int -> IO [Int]
easyComputerTurn board line quantityToRemove = do
  let lineOldValue = getLineVal board line
  if lineOldValue /= 0
    then do
      let lineNewValue = lineOldValue - quantityToRemove
      let newBoard = setLineVal board line lineNewValue
      return newBoard
    else do
      let newLine = line + 1
      easyComputerTurn board newLine quantityToRemove

--main game loop
gameLoop :: [Int] -> Bool -> IO ()
gameLoop board player = do
  putStrLn "Board:"
  printBoard board
  printWhoIsPlaying player
  if (player == False)
    then do
      board <- computerTurn board True
      if checkWin board
        then putStrLn "Computer Wins!"
        else gameLoop board (not player)
    else do
      putStrLn "Enter the line number"
      putStr "> "
      number <- getLine
      let lineNumber = read number :: Int
      putStrLn "Enter a how many sticks to take"
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
                      putStrLn "You win!"
                    else do
                      gameLoop newBoard (not player)
                else do
                  putStrLn "Invalid move, quantity entered is greater than the number of sticks in this line"
                  gameLoop board player
            else do
              putStrLn "Invalid line number, please select a number between 0 and 3"
              gameLoop board player

main = gameMenu

{-Board functions-}
--gets how many sticks are in a line
getLineVal :: [Int] -> Int -> Int
getLineVal board n = board !! n

--updates the number of sticks in a line
setLineVal :: [Int] -> Int -> Int -> [Int]
setLineVal board n val =
  let (x, _ : y) = splitAt n board
   in x ++ [val] ++ y

--checks if the game is over
checkWin :: [Int] -> Bool
checkWin board = all (== 0) board

--displays the board on the screen
printBoard :: [Int] -> IO () --TODO print the complete line
printBoard b = do
  Control.Monad.when (not (null b)) $ do
    let line = "|" ++ (show (head b)) ++ "|"
    putStrLn line
    printBoard (tail b)