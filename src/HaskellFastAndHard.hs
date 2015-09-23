module HaskellFastAndHard where

main :: IO ()
main = do
    putStrLn "What is your name?"
    name <- getLine
    putStrLn $ name ++ "! This is a very nice name."
    putStrLn "Where do you live?"
    city <- getLine
    putStrLn $ "Hey, I Love " ++ city ++ "!"

