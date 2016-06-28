import Core
import Algo
import AI

main :: IO ()
main = do
    print $ runFullGame a PX
    where
        a = emptyBoard
        b = makeMove a PX
        c = makeMove b PO
        d = makeMove c PX
        e = makeMove d PO
