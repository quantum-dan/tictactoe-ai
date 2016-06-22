module Algo (
    winWeight,
    countWins,
    findAllMoves,
    getWinWeight,
    getTotalWinWeight,
    simulateOpponent,
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

simulateOpponent :: Board -> Player -> [Board]
simulateOpponent board player = [ b | b <- findAllMoves board p, checkContinue b ]
    where
        p = case player of
            PX  -> PO
            PO  -> PX
        checkContinue b = case determineWin b of -- Check that opponent has not won; if opponent has won we do not need to include it as a loss is 0
            Nothing     -> True
            (Just N)    -> True
            _           -> False

getTotalWinWeight :: Board -> Player -> Int
getTotalWinWeight board player = case determineWin board of
    Nothing     -> sum [ getTotalWinWeight b player | b <- simulateOpponent board player ]
    _           -> winWeight board player
