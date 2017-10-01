module Player.TeamSinister (playerTeamSinister, teamMembers) where

import qualified Types
import qualified Misc

data Strategy = Attack | Defend
    deriving (Eq, Show)

teamMembers :: String
teamMembers = "Davis Silverman"

playerTeamSinister :: Types.Player
playerTeamSinister = Player getStrategizedMove "Sinistersnare"

getStrategizedMove :: Types.Tile -> Types.Board -> IO Types.Move
getStrategizedMove t b =
    case getStrategy t b of
        Attack -> attack t b
        Defend -> defend t b


-- Decides on if a chain is dangerous or not.
--  Generally, a dangerous chain should be defended against.
dangerousChain :: Types.Tile -> [Types.Move] -> Bool
dangerousChain t c =
    -- (length bestEnemyChain) > 2 && growableBy (5 - length bestEnemyChain)
    False

getStrategy :: Types.Tile -> Types.Board -> Strategy
getStrategy t b =
    let bestEnemyChain = getBestChain (Types.flipTile t) b
    -- if there is a chain bigger than 2, there is probably a problem.
    if dangerousChain bestEnemyChain && (! aboutToWin t b)


-- TODO
--  make sure where you place, you will eventually be able to get 5 in a row
--      (unless blocked off at some point in the future, but thats unknowable...)
-- Boop
attack :: Types.Tile -> Types.Board -> IO Types.Move
attack t b = (0,0)

defend :: Types.Tile -> Types.Board -> IO Types.Move
defend t b = (0,0) -- TODO

