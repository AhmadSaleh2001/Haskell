myReadLine :: IO String
myReadLine = do 
 x <- getChar
 if x == '\n' then
  return []
 else 
  do 
   xs <- myReadLine
   return (x:xs)

enterStringPrompt :: String -> IO String
enterStringPrompt msg = do
 putStr msg
 line <- myReadLine
 return line


giveHint :: String -> String -> String
giveHint [] gword = ['\n']
giveHint (x:xs) [] = ('-':(giveHint xs []))
giveHint (x:xs) (y:ys) = do
 if x == y then
  (x:(giveHint xs ys))
 else
  ('-':(giveHint xs ys))

 

startGuessing :: String -> IO ()
startGuessing word = do
 gword <- enterStringPrompt "guess the word: " 
 if word == gword then
  putStr "winner winner checkin dinner\n"
 else
  putStr (giveHint word gword);
  startGuessing word
  

startGame :: IO ()
startGame = do
 word <- enterStringPrompt "enter word: "
 startGuessing word
