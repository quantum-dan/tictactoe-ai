-- Module implementing the actual AI

module AI (
    makeMove,
    runFullGame,
    ) where

import Core
import Algo

makeMove :: Board -> Player -> Board -- Makes the move with the highest total win weight, and returns the updated board
makeMove board player = maxBoard boardList
    where
        boardList = [ (getTotalWinWeight b player, b) | b <- findAllMoves board player ]
        maxBoard boards = snd $ maxBoard' boards
            where
                maxBoard' (b:boards) = if winWeight (snd b) player == 2
                    then b
                    else
                        let b' = maxBoard' boards in
                            if fst b > fst b' then b
                            else b'
                maxBoard' [] = (0, emptyBoard)

runFullGame :: Board -> Player -> Board
runFullGame board player = if checkGameOver board then board
    else runFullGame (makeMove board player) $ switchPlayer player
