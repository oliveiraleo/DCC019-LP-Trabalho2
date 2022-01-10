--Nim game

import Control.Monad --used in printBoard
import System.Random --used in getRandomInt

board :: [Int]
board = [1, 3, 5, 7]

{-Utilities-}
--returns a random integer between x and y
getRandomInt :: Int -> Int -> IO Int
getRandomInt x y = getStdRandom (randomR (x, y))

--converts a decimal number to a binary number
dec2bin :: Int -> [Int]
dec2bin 0 = [0]
dec2bin n = reverse (dec2binAux n)

dec2binAux 0 = []
dec2binAux n
  | n `mod` 2 == 1 = 1 : dec2binAux (n `div` 2)
  | n `mod` 2 == 0 = 0 : dec2binAux (n `div` 2)

--converts binary to integer -- lista 4 exercio 3
bin2int :: [Int] -> Int
bin2int = foldl (\x y -> x * 10 + y) 0

--extracts all digits from a number
getEachDigit :: Int -> [Int]
getEachDigit 0 = []
getEachDigit n = getEachDigitAux n : getEachDigit (n `div` 10)

getEachDigitAux 0 = 0
getEachDigitAux n = n `mod` 10

--checks if all digits of a number are even
checkAllEven :: [Int] -> Bool
checkAllEven [] = True
checkAllEven (x:xs)
  | even x = checkAllEven xs
  | otherwise = False

--converts decimal to binary and then to integer
--dec2bin2int :: Int -> Int
--dec2bin2int = bin2int . dec2bin

--converts a decimal number to a binary number with length = 3
dec2binlen3 :: Int -> [Int]
dec2binlen3 n = do
  let bin = dec2bin n
  if length bin < 3
    then do
      let bin' = replicate (3 - length bin) 0 ++ bin
      bin'
    else do
      bin

--prints whose turn it is
printWhoIsPlaying :: Bool -> IO ()
printWhoIsPlaying p = do
  if p
    then putStrLn "-> Human Player is playing"
    else putStrLn "-> Computer is playing"

{-Game initial menus-}
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
  if d == 0 -- quits the game
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
      --evaluate difficulty levels
      if level == 1 --easy
        then gameLoop board True False --human starts, machine is in easy mode
        else
          if level == 2 --hard
            then gameLoop board False True --human do not start, machine is in hard mode
            else
              if level == 99 -- quits the game
                then putStrLn "Bye!"
                else putStrLn "Invalid option"
    else putStrLn "Bye!"

{-Game Logic-}
--implements the computer's move
computerTurn :: [Int] -> Bool -> IO [Int]
computerTurn board godMode = do
  if godMode
    then do
      putStrLn "Not ready yet" --TODO: implement the computer turn on hard mode
      return board
    else do
      easyComputerTurn board --makes a move taking a random number of stickers at a time on a random line

--implements the easy computer move
easyComputerTurn :: [Int] -> IO [Int]
easyComputerTurn board = do
  line <- getRandomInt 0 3
  quantityToRemove <- getRandomInt 1 7
  let lineOldValue = getLineVal board line
  if (lineOldValue /= 0) && (quantityToRemove <= lineOldValue) -- checks if the line still has stickers and if the quantity to remove is less than the line's value
    then do
      -- if yes, removes the random number of stickers
      let lineNewValue = lineOldValue - quantityToRemove
      let newBoard = setLineVal board line lineNewValue
      return newBoard
    else do
      -- if no, tries again with other values
      easyComputerTurn board

--implements the hard computer move --TODO
--hardComputerTurn :: [Int] -> IO [Int]

--main game loop
gameLoop :: [Int] -> Bool -> Bool -> IO ()
gameLoop board player machineGodMode = do
  putStrLn "\nBoard:"
  printBoard board
  putStrLn "" --displays a new line for better readability
  printWhoIsPlaying player
  if (player == False) --computer's turn
    then do
      board <- computerTurn board machineGodMode
      if checkWin board
        then do
          putStrLn "Board:\n"
          printBoard board
          putStrLn "\n\nOh no!...\n\n---The Computer wins this time---\n"
        else gameLoop board (not player) machineGodMode
    else do
      -- player's turn
      putStrLn "Enter the line number you choose:"
      putStr "> "
      number <- getLine
      let lineNumber = read number :: Int
      putStrLn "Enter a how many sticks to take:"
      putStr "> "
      quantity2 <- getLine
      let quantity = read quantity2 :: Int
      if quantity <= 0 --checks if the quantity is valid
        then do
          putStrLn "Invalid quantity, please select at least 1 stick on each turn"
          gameLoop board player machineGodMode
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
                      putStrLn "Board:\n"
                      printBoard newBoard
                      putStrLn "\n\nOh yeah!\n\n!!!You win!!!\n"
                    else do
                      gameLoop newBoard (not player) machineGodMode
                else do
                  putStrLn "Invalid move, quantity entered is greater than the number of sticks in this line"
                  gameLoop board player machineGodMode
            else do
              putStrLn "Invalid line number, please select a number between 0 and 3"
              gameLoop board player machineGodMode

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

--checks if the move was perfect
checkPerfectMove :: [Int] -> Bool
checkPerfectMove board = do
  let item1 = head board
  let item2 = board !! 1
  let item3 = board !! 2
  let item4 = board !! 3

  let binItem1 = dec2binlen3 item1
  let binItem2 = dec2binlen3 item2
  let binItem3 = dec2binlen3 item3
  let binItem4 = dec2binlen3 item4

  let columnSum = bin2int binItem1 + bin2int binItem2 + bin2int binItem3 + bin2int binItem4

  let columnSumDigits = getEachDigit columnSum

  checkAllEven columnSumDigits

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