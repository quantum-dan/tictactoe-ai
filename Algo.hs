module Algo (
    winWeight,
    countWins,
    findAllMoves,
    getWinWeight,
    getTotalWinWeight,
    simulateOpponent,
    findAllOutcomes,
    checkGameOver,
    ) where
import Core

winval = 2
tieval = 1
loseval = 0

winWeight :: Board -> Player -> Int
winWeight board player = case determineWin board of
    Nothing     -> 0 -- Game is not over
    (Just N)    -> tieval -- Tie
    (Just p)    -> if p == player then winval else loseval -- Lost

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

checkGameOver :: Board -> Bool -- Determine if the game is over, based on determineWin
checkGameOver board = case determineWin board of
    Nothing     -> False
    _           -> True

findAllOutcomes :: Board -> Player -> [Board] -- find all possible outcomes for a given board if it is a given player's move
findAllOutcomes board player = if checkGameOver board then [board]
    else endgameOutcomes ++ continueOutcomes
    where
        allOutcomes = findAllMoves board player
        endgameOutcomes = [ outcome | outcome <- allOutcomes, checkGameOver outcome ]
        continueOutcomes = concat [ findAllOutcomes outcome $ switchPlayer player | outcome <- allOutcomes, not (checkGameOver outcome) ]

getTotalWinWeight :: Board -> Player -> Int
getTotalWinWeight board player = sum [ winWeight b player | b <- findAllOutcomes board player ]
