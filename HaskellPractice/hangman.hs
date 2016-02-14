module hangman where

hangman :: IO ()
hangman  =
  do putStrLn "Think of a word: "
     word <- sgetLine
     putStrLn "try to guess it:"
     guess word

sgetLine :: IO String
sgetLine = do x <- getCh
              if x == '\n' then
                 do putChar x
                    return []
               else
                 do putChar '_'
                    xs <- sgetLine
                    return (x:xs)

-- getCh reads a character from the keyboard without echoing it to the screen
getCh :: IO Char

{- the function guess is the main loop, which requests and processes
guesses until the game ends. -}

guess     :: String -> IO ()
guess word =
   do putStr "> "
      xs <- getLine
      if xs == word then
         putStrLn "you got it!"
       else
         do putStrLn (diff word xs)
            guess word


