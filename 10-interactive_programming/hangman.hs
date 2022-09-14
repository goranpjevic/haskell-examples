--hangman game

import System.IO

-- get a character but don't echo it to the output
getSecretChar :: IO Char
getSecretChar = do hSetEcho stdin False
                   x <- getChar
                   hSetEcho stdin True
                   return x

getSecretLine :: IO String 
getSecretLine = do x <- getSecretChar
                   if x == '\n' then
                      do putChar x
                         return []
                   else
                      do putChar '-'
                         xs <- getSecretLine
                         return (x:xs)

match :: String -> [Bool] -> [Char]
match [] [] = []
match (c:cs) (b:bs) = (if b then c else '_') : match cs bs

-- game loop
playIter :: String -> [Char] -> Int -> IO Int
playIter word gs wg = do putStr "guess a letter: "
                         g <- getChar
                         putChar '\n'
                         let ng = gs ++ [g]
                         -- number of new wrong guesses
                         let nwg = wg + if or (map (==g) word) then 0 else 1
                         -- wrong or correct guesses for each character
                         let nb = map (\c -> or (map (==c) ng)) word
                         putStrLn (match word nb)
                         putStr "number of wrong guesses: "
                         putStrLn (show nwg)
                         if and nb then
                            do putStrLn "win"
                               return 1
                         else if nwg >= 5 then
                                 do putStrLn "game over"
                                    return (-1)
                              else
                                 playIter word ng nwg

play :: Int -> IO ()
play score = do putStr "enter a word to be guessed: "
                word <- getSecretLine
                putStrLn "guess the word:"
                win <- playIter word [] 0
                putStr "total score: "
                putStrLn (show (score + win))
                putStr "play again? [y/N] "
                yn <- getChar
                putChar '\n'
                if yn == 'y' then
                   play (score + win)
                else
                   return ()

hangman :: IO ()
hangman = play 0
