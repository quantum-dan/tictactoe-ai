module Algo (
    winWeight,
    countWins,
    findAllMoves,
    getWinWeight
    ) where
import Core

winWeight :: Board -> Player -> Int
winWeight board player = case determineWin board of
    Nothing     -> 0 -- Game is not over
    (Just N)    -> 1 -- Tie
    (Just p)    -> if p == player then 2 else 0 -- Lost

countWins :: [Board] -> Player -> Int
countWins boards player = sum [ winWeight board player | board <- boards ]

findAllMoves :: Board -> Player -> [Board]
findAllMoves board player = [ setByPosition board coords place | coords <- clist, getByPosition board coords == E ]
    where
        clist = zip [1, 1, 1, 2, 2, 2, 3, 3, 3] [1, 2, 3, 1, 2, 3, 1, 2, 3]
        place = getPlace player

getWinWeight :: Board -> Player -> Int
getWinWeight board player = countWins ( findAllMoves board player ) player
