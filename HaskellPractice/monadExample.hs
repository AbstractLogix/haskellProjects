welcomeFunc = putStrLn "What is your name?" >>= (\_ -> getLine) >>= (\name -> putStrLn("Welcome, " ++ name ++ "!"))

