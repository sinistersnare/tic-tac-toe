module Main where

import Types
import Checks
import Misc

import Player.Computer (playerComputer)
import Player.Human    (playerHuman)
import Player.MinMax (playerMinMax)
-- import Player.BestNext (playerBestNext)

player1, player2 :: Player
player1 = playerMinMax
player2 = playerMinMax

main :: IO ()
main = do
    putStrLn "This is classic tic tac toe game."
    rounds  <- prompt "How many rounds should we play?"
    score   <- playRounds (read rounds) player1 player2
    putStrLn $ showFinalScore score


playRounds :: Int -> Player -> Player -> IO Score
playRounds rounds player1 player2 =
  foldM (playRound pi1 pi2) [(pi1,0),(pi2,0)] [1..rounds]
  where
    pi1 = PI player1 X 1
    pi2 = PI player2 O 2

playRound :: PlayerInfo -> PlayerInfo -> Score -> Int -> IO Score
playRound p1 p2 score i = do
   putStrLn ("Score:: " ++ showScore score)
   putStrLn ("Round " ++ show i ++ "!")
   putStrLn ((if (i `mod` 2 == 0) then show p2 else show p1)  ++ " plays first")
   result <- if (i `mod` 2 == 0) then play p2 p1 emptyBoard else play p1 p2 emptyBoard
   case result of
      Just p  -> putStrLn (show p ++ " wins!\n\n") >> return (incr p score)
      Nothing -> putStrLn "Its a tie!\n\n" >> return score


play :: PlayerInfo -> PlayerInfo -> Board -> IO (Maybe Winner)
play pi1@(PI p1 t1 _) pi2 board = do
  move <- (playerMove p1) t1 board
  case putMaybe board t1 move of
    Nothing -> putStrLn "Invalid move." >> return (Just pi2)
    Just b  -> do putStrLn $ showBoard b
                  if tileWins b t1
                    then return (Just pi1)
                    else if checkFull b
                    then return Nothing
                    else play pi2 pi1 b
