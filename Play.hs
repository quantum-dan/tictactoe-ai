-- Basic Tic Tac Toe game against the AI, for testing

import Core
import Algo
import AI

runCycle :: Board -> Board
runCycle board = if checkGameOver board
    then board
    else
        makeMove board PO

runMove :: Board -> IO (Bool, Player, Board)
runMove board = do
    if checkGameOver board then do
        return (True,
                case determineWin board of
                    (Just p)    -> p
                    _           -> N,
                board)
    else do
        putStrLn "Board:"
        print board
        putStrLn "Enter move coordinates in format (x, y):"
        coordStr <- getLine
        let coords = read coordStr
        if placeIsFull $ getByPosition board coords then do
            putStrLn "Selected position is already occupied.  Please try again.\n"
            runMove board
        else do
            let board' = setByPosition board coords X
            runMove $ runCycle board'

main :: IO ()
main = do
    putStrLn "Tic Tac Toe game: You are player X.\n"
    let board = emptyBoard
    (_, player, board') <- runMove board
    putStrLn $ case player of
        PO  -> "Sorry, you lose"
        PX  -> "Congratulations, you win"
        _   -> "It's a tie"
    putStrLn "Final board:\n"
    print board'
