-- nim game

-- get the string representation of the board
getBoardString :: [Int] -> Int -> String
getBoardString [] _ = []
getBoardString (s:b) i = oneLine ++ getBoardString b (i+1)
                         where
                           stars = concat [ " *" | x <- [1..s]]
                           oneLine = (show i) ++ ":" ++ stars ++ "\n"

printBoard :: [Int] -> IO ()
printBoard b = putStrLn (getBoardString b 1)

updateBoard :: [Int] -> Int -> Int -> [Int]
updateBoard (s:b) 1 nsr = (s - nsr) : b
updateBoard (s:b) r nsr = s : updateBoard b (r-1) nsr

removeStars :: [Int] -> IO [Int]
removeStars b = do putStr "row to remove stars from: "
                   rStr <- getLine
                   let r = read rStr :: Int
                   putStr ("how many stars to remove from row " ++ (show r) ++ ": ")
                   nsrStr <- getLine
                   let nsr = read nsrStr :: Int
                   putChar '\n'
                   if (r <= 0) || (r > length b) || (b !! (r-1)) < nsr then
                      do putStrLn "invalid inputs"
                         removeStars b
                   else
                      return (updateBoard b r nsr)

-- game loop
playIter :: [Int] -> Int -> IO (Int,Int)
playIter b p = do printBoard b
                  putStrLn ("player " ++ (show p) ++ " move")
                  nb <- removeStars b
                  if and (map (==0) nb) then
                     return (if p == 1 then (1,0) else (0,1))
                  else
                     playIter nb (if p == 1 then 2 else 1)

play :: Int -> Int -> IO ()
play sc1 sc2 = do let board = reverse [1..5]
                  (win1,win2) <- playIter board 1
                  let nsc1 = sc1 + win1
                  let nsc2 = sc2 + win2
                  putStrLn "score:"
                  putStrLn ("player 1: " ++ (show nsc1))
                  putStrLn ("player 2: " ++ (show nsc2))
                  putStr "play again? [y/N] "
                  yn <- getChar
                  putChar '\n'
                  if yn == 'y' then
                     play nsc1 nsc2
                  else
                     return ()

nim :: IO ()
nim = play 0 0
