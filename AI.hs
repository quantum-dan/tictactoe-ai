-- Module implementing the actual AI

module AI (
    makeMove,
    runFullGame,
    ) where

import Core
import Algo

movesAhead = 3

makeMove :: Board -> Player -> Board -- Makes the move with the highest total win weight, and returns the updated board
makeMove board player = maxBoard boardList'
    where
        boardList = [ (getTotalWinWeight b player, b) | b <- findAllMoves board player ]
        boardList' = [ (getTotalPatternWeight b player player movesAhead, b) | b <- findAllMoves board player ]
        maxBoard boards = snd $ maxBoard' boards
            where
                maxBoard' (b:boards) = if checkPattern (snd b) player WinP
                    then b
                    else
                        let b' = maxBoard' boards in
                            if fst b > fst b' then b
                            else b'
                maxBoard' [] = (-10000000000, emptyBoard)

runFullGame :: Board -> Player -> Board
runFullGame board player = if checkGameOver board then board
    else runFullGame (makeMove board player) $ switchPlayer player
