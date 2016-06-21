import Core
import Algo

main :: IO ()
main = do
    print $ determineWin a
    print $ determineWin b
    print $ determineWin c
    print $ determineWin d
    print $ countWins [a, b, c, d] PX
    print $ getByPosition a (2, 2)
    print $ getByPosition b (3, 1)
    where
        a = Board (Row X X X) (Row X O O) (Row E E X)
        b = Board (Row O X X) (Row X O X) (Row O E O)
        c = Board (Row X O X) (Row O X O) (Row O X O)
        d = Board (Row E X E) (Row E O E) (Row X E O)
