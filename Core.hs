-- Core stuff: Tic Tac Toe struct, win conditions
module Core where

data Place = X | O | E
    deriving (Show, Eq)
-- X, O or Empty

data Player = PX | PO | N
    deriving (Show, Eq)
-- N is to be used only for a full board where no one has won

getPlace :: Player -> Place
getPlace player = case player of
    PX -> X
    PO -> O
    N -> E

data Row = Row {
    left    :: Place,
    mid     :: Place,
    right   :: Place
}

instance Show Row where
    show (Row l m r) = (show l) ++ " " ++ (show m) ++ " " ++ (show r)

data Board = Board {
    top     :: Row,
    middle  :: Row,
    bottom  :: Row
}

instance Show Board where
    show (Board t m b) = (show t) ++ "\n\n" ++ (show m) ++ "\n\n" ++ (show b) ++ "\n"

getByPosition :: Board -> (Int, Int) -> Place
getByPosition (Board t m b) (x, y) = selector (case y of
    1 -> t
    2 -> m
    _ -> b)
    where
        selector = case x of
            1 -> left
            2 -> mid
            3 -> right

setByPosition :: Board -> (Int, Int) -> Place -> Board
setByPosition (Board t m b) (x, y) place = Board t' m' b'
    where
        substitute (Row l m r) x = Row
            (if x == 1 then place else l)
            (if x == 2 then place else m)
            (if x == 3 then place else r)
        t' = if y == 1 then substitute t x else t
        m' = if y == 2 then substitute m x else m
        b' = if y == 3 then substitute b x else b

placeIsFull :: Place -> Bool
placeIsFull E = False
placeIsFull _ = True

rowIsFull :: Row -> Bool
rowIsFull (Row l m r) = placeIsFull l && placeIsFull m && placeIsFull r

boardIsFull :: Board -> Bool
boardIsFull (Board t m b) = rowIsFull t && rowIsFull m && rowIsFull b

findDiag :: Board -> Place -> Bool
findDiag (Board t m b) place =
    (mid m == place) &&
    (((left t == place) && (right b == place)) ||
        ((right t == place) && (left b == place)))

findRow :: Board -> Place -> Bool
findRow (Board t m b) place = checkRow t || checkRow m || checkRow b
    where
        checkRow (Row l m r) = (l == place) && (m == place) && (r == place)

findColumn :: Board -> Place -> Bool
findColumn (Board t m b) place =
    ((left t == place) && (left m == place) && (left b == place)) ||
    ((mid t == place) && (mid m == place) && (mid b == place)) ||
    ((right t == place) && (right m == place) && (right b == place))

findPath :: Board -> Player -> Bool
findPath board player = (findDiag board p) || (findRow board p) || (findColumn board p)
    where
        p = getPlace player

determineWin :: Board -> Maybe Player -- Just Player if won or full, Nothing if no one has won and there are free spots
determineWin board
    | findPath board PX = Just PX
    | findPath board PO = Just PO
    | boardIsFull board = Just N
    | otherwise         = Nothing
