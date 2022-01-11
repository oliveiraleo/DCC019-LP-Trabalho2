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
dec2bin n = reverse (dec2binAux n) --reverse to avoid adding 0s at the end and to get the right order

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
checkAllEven (x : xs)
  | even x = checkAllEven xs
  | otherwise = False

--converts a decimal number to a binary number with length = 3
dec2binlen3 :: Int -> [Int]
dec2binlen3 n = do
  let bin = dec2bin n
  if length bin < 3
    then do
      let bin' = replicate (3 - length bin) 0 ++ bin --adds zeros to the left if necessary
      bin'
    else do
      bin

--prints whose turn it is
printWhoIsPlaying :: Bool -> IO ()
printWhoIsPlaying p = do
  if p
    then putStrLn "-> Human Player is playing"
    else putStrLn "-> Computer is playing"

--displays all the game information divided in 3 "pages"
printGameInfo :: Int -> IO ()
printGameInfo n
  | n == 0 = do
    putStrLn "\n|------------------------|"
    putStrLn "|     Game information   |"
    putStrLn "|------------------------|"
    putStrLn "- The game is a two-player game."
    putStrLn "- The game is played on a board with 4 piles of stickers on each one."
    putStrLn "- The goal is to remove all the stickers from the board."
    putStrLn "\n=> Rules:\n"
    putStrLn "- The game is played in rounds, one player plays per turn."
    putStrLn "- The player who takes the last sticker from the board wins the game.\n"
    nextPage <- askForNextPage n
    printGameInfo nextPage
  | n == 1 = do
    putStrLn "\n=> Back-end:\n"
    putStrLn "- The board is represented by a list of 4 integers, each representing the number of stickers on each pile."
    putStrLn "- The line number is the index of the list, starting from 0."
    putStrLn "- The player is represented by a boolean, True for the human player and False for the machine."
    putStrLn "- There are two levels available: easy and hard."
    putStrLn "--Easy mode: the human player starts the game."
    putStrLn "--Hard mode: the machine starts the game and then, if possible, only makes perfect moves.\n"
    nextPage <- askForNextPage n
    printGameInfo nextPage
  | n == 2 = do
    putStrLn "\n=> Board:\n"
    putStrLn "- The board is displayed containing:"
    putStrLn "--The number of the line of the board;"
    putStrLn "--The number of stickers on each pile; and"
    putStrLn "--The simulated physical representation of each sticker."
    putStrLn "\n=> Example:\n"
    putStrLn "- If the board is the list [1, 3, 5, 7], it will be represented as below:\n"
    putStrLn "0 ( 1 ): |"
    putStrLn "1 ( 3 ): |||"
    putStrLn "2 ( 5 ): |||||"
    putStrLn "3 ( 7 ): |||||||"
    putStrLn "\nGood luck!\n"
    nextPage <- askForNextPage n
    printGameInfo nextPage
  | otherwise = gameMenu --goes back to the main menu when finished

--asks the user if he wants to see the next page
askForNextPage :: Int -> IO Int
askForNextPage pageNumber = do
  putStrLn "Press enter to continue..."
  getLine
  return (pageNumber + 1)

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
  putStrLn "Welcome to Nim!\n"
  putStrLn "Are you ready?"
  putStrLn "1. Yes, let's play!"
  putStrLn "9. Hold on, give me some information about the game first"
  putStrLn "0. Not yet, exit"
  putStr "> "
  option <- getLine
  let selectedOption = (read option :: Int)
  if selectedOption == 1
    then do
      --evaluate difficulty levels
      level <- selectDifficulty
      if level == 1 --easy
        then gameLoop board True False --human starts, machine is in easy mode
        else
          if level == 2 --hard
            then gameLoop board False True --the human does not start, machine is in hard mode
            else
              if level == 99 -- quits the game
                then putStrLn "Bye!"
                else putStrLn "Invalid option"
    else
      if selectedOption == 9
        then do
          printGameInfo 0 --prints the game information, starting from the first page
        else putStrLn "Bye!"

{-Game Logic-}
--implements the computer's move
computerTurn :: [Int] -> Bool -> IO [Int]
computerTurn board godMode = do
  if godMode --checks if it is in the hard mode
    then do
      if isPerfectBoard board
        then randomComputerTurn board
        else perfectComputerTurn board
    else randomComputerTurn board --makes a move taking a random number of stickers at a time on a random line

--implements the easy computer move
randomComputerTurn :: [Int] -> IO [Int]
randomComputerTurn board = do
  line <- getRandomInt 0 3
  quantityToRemove <- getRandomInt 1 7
  let lineOldValue = getLineValue board line
  if (lineOldValue /= 0) && (quantityToRemove <= lineOldValue) -- checks if the line still has stickers and if the quantity to remove is less than the line's value
    then do
      -- if yes, removes the random number of stickers
      let lineNewValue = lineOldValue - quantityToRemove
      let newBoard = setLineValue board line lineNewValue
      return newBoard
    else do
      -- if no, tries again with other values
      randomComputerTurn board

--implements the hard computer move
perfectComputerTurn :: [Int] -> IO [Int]
perfectComputerTurn board = do
  newBoard <- randomComputerTurn board
  if isPerfectBoard newBoard
    then return newBoard
    else perfectComputerTurn board

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
          putStrLn "Going back to the main menu...\n"
          gameMenu
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
          let lineVal = getLineValue board lineNumber
          if lineNumber >= 0 && lineNumber < 4 --checks if the line exists
            then do
              if lineVal >= quantity --checks if the stickers exist
                then do
                  let newLineValue = subtract quantity lineVal
                  let newBoard = setLineValue board lineNumber newLineValue
                  if checkWin newBoard
                    then do
                      putStrLn "Board:\n"
                      printBoard newBoard
                      putStrLn "\n\nOh yeah!\n\n!!!You win!!!\n"
                      putStrLn "Going back to the main menu...\n"
                      gameMenu
                    else do
                      gameLoop newBoard (not player) machineGodMode
                else do
                  putStrLn "Invalid move, quantity entered is greater than the number of sticks in this line"
                  gameLoop board player machineGodMode
            else do
              putStrLn "Invalid line number, please select a number between 0 and 3"
              gameLoop board player machineGodMode

main = gameMenu

--checks if the game is over
checkWin :: [Int] -> Bool
checkWin board = all (== 0) board --if the sticks are all gone, the game is over

{-Board functions-}
--gets how many sticks are in a line
getLineValue :: [Int] -> Int -> Int
getLineValue board n = board !! n

--updates the number of sticks in a line
setLineValue :: [Int] -> Int -> Int -> [Int]
setLineValue board n val =
  let (x, _ : y) = splitAt n board
   in x ++ [val] ++ y

--checks if the move was perfect
isPerfectBoard :: [Int] -> Bool
isPerfectBoard board = do
  --split the board in 4 lines
  let item1 = head board
  let item2 = board !! 1
  let item3 = board !! 2
  let item4 = board !! 3
  --checks how much sticks are in each line and converts them to binary
  let binItem1 = dec2binlen3 item1
  let binItem2 = dec2binlen3 item2
  let binItem3 = dec2binlen3 item3
  let binItem4 = dec2binlen3 item4
  --get the digits together
  let columnSum = bin2int binItem1 + bin2int binItem2 + bin2int binItem3 + bin2int binItem4
  --puts all digits in a list
  let columnSumDigits = getEachDigit columnSum
  --checks if all digits are even numbers
  checkAllEven columnSumDigits

--displays the board on the screen
printBoard :: [Int] -> IO ()
--old version
{-printBoard b = do
  Control.Monad.when (not (null b)) $ do
    let line = "|" ++ (show (head b)) ++ "|"
    putStrLn line
    printBoard (tail b)-}
--new version
printBoard board = putStr $ unlines [show rowNumber ++ " " ++ "( " ++ show stickers ++ " )" ++ ": " ++ replicate stickers '|' | (stickers, rowNumber) <- zip board [0 .. length board]]
