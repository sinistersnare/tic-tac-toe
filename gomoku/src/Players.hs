module Players (players) where
import Types
import Player.BestNext (playerBestNext)
import Player.Computer (playerComputer)
import Player.Human (playerHuman)
import Player.TeamSinister (playerTeamSinister)
players :: [(String, Player)]
players = [
  ("BestNext", playerBestNext),("Computer", playerComputer),("Human", playerHuman),("TeamSinister", playerTeamSinister)
  ]
