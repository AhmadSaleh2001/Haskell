myReadLine :: IO String
myReadLine = do
 x <- getChar
 if x == '\n' then
  return []
 else
  do
   xs <- myReadLine
   return (x:xs)


printWinner :: Int -> IO()
printWinner x = do
 if x == 1 then
  putStr "player 1 is the winner\n"
 else
  putStr "player 2 is the winner\n"

printToPlayerToInput :: Int -> IO()
printToPlayerToInput x = do
 if x == 1 then
  putStr "player 1: \n"
 else
  putStr "player 2: \n"

enterStringPrompt :: String -> IO()
enterStringPrompt msg = do
 putStr msg


printStars :: Int -> IO()
printStars x = do
 if x == 0 then
  putChar '\n'
 else
  do
   putChar '*'
   printStars (x-1)

intToChar :: Int -> Char
intToChar x
 | x == 0 = '0'
 | x == 1 = '1'
 | x == 2 = '2'
 | x == 3 = '3'
 | x == 4 = '4'
 | x == 5 = '5'
 | x == 6 = '6'
 | x == 7 = '7'
 | x == 8 = '8'
 | x == 9 = '9'

charToInt :: Char -> Int
charToInt x
 | x == '0' = 0
 | x == '1' = 1
 | x == '2' = 2
 | x == '3' = 3
 | x == '4' = 4
 | x == '5' = 5
 | x == '6' = 6
 | x == '7' = 7
 | x == '8' = 8
 | x == '9' = 9


isGameEnd :: [Int] -> Bool
isGameEnd [] = True
isGameEnd (x:xs) = (x == 0) && isGameEnd xs

validStep :: [Int] -> Int -> Int -> Bool
validStep arr rowIdx subtract
 | arr !! rowIdx >= subtract = True
 | otherwise = False

assign :: [Int] -> Int -> Int -> [Int]
assign [] idx subtract = []
assign (x:xs) idx subtract
 | idx == 0 = (x - subtract):xs
 | otherwise = x:(assign xs (idx - 1) subtract)


getLen :: [Int] -> Int
getLen [] = 0
getLen (x:xs) = 1 + getLen xs

printNumber :: Int -> IO()
printNumber x = do
 putChar (intToChar x)

printArrayOfStars :: [Int] -> Int -> IO()
printArrayOfStars [] l = do
 putChar '\n'
printArrayOfStars (x:xs) l = do
 putChar (intToChar (5-l))
 putChar ':'
 putChar ' '
 printStars x
 printArrayOfStars xs (l-1)
 
startGame :: [Int] -> Int -> IO()
startGame arr playerNumber = do

 printArrayOfStars arr (getLen arr)

 if isGameEnd arr then
  printWinner (3 - playerNumber)
 else
  do
  printToPlayerToInput playerNumber
    
  enterStringPrompt "enter row number: "
  rowNumber <- getChar

  getChar
  
  enterStringPrompt "enter value to delete: "
  valueToSubtract <- getChar
  getChar

  let intRowNumber = charToInt rowNumber
  let intValueToSubtract = charToInt valueToSubtract

  if (validStep arr intRowNumber intValueToSubtract) then
    startGame (assign arr intRowNumber intValueToSubtract) (3 - playerNumber)
  else
    startGame arr playerNumber

 

main :: IO ()
main = do
 let arr = [5, 4, 3, 2, 1]
 startGame arr 1
 