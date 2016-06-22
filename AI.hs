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
                maxBoard' (b:boards) = let b' = maxBoard' boards in
                    if fst b > fst b' then b
                    else b'
                maxBoard' [] = (0, Board (Row E E E) (Row E E E) (Row E E E))

runFullGame :: Board -> Player -> Board
runFullGame board player = if checkGameOver board then board
    else runFullGame (makeMove board player) $ switchPlayer player
