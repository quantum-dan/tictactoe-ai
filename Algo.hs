module Algo (
    winWeight,
    countWins
    ) where
import Core

winWeight :: Board -> Player -> Int
winWeight board player = case determineWin board of
    Nothing     -> 0 -- Game is not over
    (Just N)    -> 1 -- Tie
    (Just p)    -> if p == player then 2 else 0 -- Lost

countWins :: [Board] -> Player -> Int
countWins boards player = sum [ winWeight board player | board <- boards ]
