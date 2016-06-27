import Core
import Algo
import AI

main :: IO ()
main = do
    {- print $ determineWin a -- Does determineWin work properly?
    print $ determineWin b
    print $ determineWin c
    print $ determineWin d
    print $ countWins [a, b, c, d] PX -- Does countWins work properly?
    print $ getByPosition a (2, 2) -- Does getByPosition work properly?
    print $ getByPosition b (3, 1)
    print a
    let e = setByPosition a (1, 2) O -- Does setByPosition work properly?
    print e
    putStrLn "--------------"
    print f
    print $ findAllMoves f PX -- Does findAllMoves work properly?
    print g
    putStrLn "---"
    print $ findAllMoves g PX
    putStrLn "---"
    print $ getWinWeight g PX
    print h
    print $ simulateOpponent h PX -- Does simulateOpponent work?
    print $ findAllMoves (simulateOpponent h PX !! 0) PX
    print h
    print $ findAllOutcomes h PX -- Does findAllOutcomes work?
    -- Does getTotalWinWeight work?
    print a
    print $ getTotalWinWeight a PX
    print b
    print $ getTotalWinWeight b PX
    print d
    print $ getTotalWinWeight d PX
    print f
    print $ getTotalWinWeight f PX
    print h
    print $ getTotalWinWeight h PX -}
    print $ getTotalPatternWeight f PX PX 9
    print $ getTotalWinWeight f PX
    where
        a = Board (Row X X X) (Row X O O) (Row E E X)
        b = Board (Row O X X) (Row X O X) (Row O E O)
        c = Board (Row X O X) (Row O X O) (Row O X O)
        d = Board (Row E X E) (Row E O E) (Row X E O)
        f = Board (Row E E E) (Row E E E) (Row E E E)
        g = Board (Row X E X) (Row E X E) (Row E E E)
        h = Board (Row X O X) (Row E O X) (Row E E E)
        i = Board (Row E E E) (Row E X E) (Row E E E)
