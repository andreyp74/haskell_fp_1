

main = do
    putStr "What is your name?\nName: "
    name <- getLine
    case length name of
        0 -> main
        otherwise -> putStrLn $ "Hi, " ++ name ++ "!"
