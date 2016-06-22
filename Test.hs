import Core
import Algo

main :: IO ()
main = do
    {- print $ determineWin a
    print $ determineWin b
    print $ determineWin c
    print $ determineWin d
    print $ countWins [a, b, c, d] PX
    print $ getByPosition a (2, 2)
    print $ getByPosition b (3, 1)
    print a
    let e = setByPosition a (1, 2) O
    print e
    putStrLn "--------------"
    print f
    print $ findAllMoves f PX
    print g
    putStrLn "---"
    print $ findAllMoves g PX
    putStrLn "---"
    print $ getWinWeight g PX -}
    print c
    print $ getTotalWinWeight c PX
    print d
    print $ getTotalWinWeight d PX
    print g
    print $ getTotalWinWeight g PX
    print a
    print $ getTotalWinWeight a PX
    where
        a = Board (Row X X X) (Row X O O) (Row E E X)
        b = Board (Row O X X) (Row X O X) (Row O E O)
        c = Board (Row X O X) (Row O X O) (Row O X O)
        d = Board (Row E X E) (Row E O E) (Row X E O)
        f = Board (Row E E E) (Row E E E) (Row E E E)
        g = Board (Row X E X) (Row E X E) (Row E E E)
