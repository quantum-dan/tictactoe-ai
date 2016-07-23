module Algo (
    winWeight,
    countWins,
    findAllMoves,
    getWinWeight,
    getTotalWinWeight,
    simulateOpponent,
    findAllOutcomes,
    checkGameOver,
    PatternType (WIN, TIE, LOSE, LIKELYWIN, LIKELYLOSE, NEUTRAL),
    Pattern (WinP, TieP, LoseP),
    getPatternType,
    checkPattern,
    patterns,
    patternWeight,
    getTotalPatternWeight,
    ) where
import Core

winval = 2
tieval = 1
loseval = 0

data PatternType = WIN | TIE | LOSE | LIKELYWIN | LIKELYLOSE | NEUTRAL
winVal :: PatternType -> Int
-- Use this to modify how aggressively the AI plays, in theory
winVal patternType = case patternType of -- Clearer than a long list of pattern matches
    WIN         -> 5
    TIE         -> 0
    LOSE        -> -5
    LIKELYWIN   -> 2
    LIKELYLOSE  -> -2
    NEUTRAL     -> 0

data Pattern = WinP | TieP | LoseP | LikelyWP | LikelyLP
patterns :: [Pattern]
patterns = [WinP, TieP, LoseP, LikelyWP, LikelyLP]

getPatternType :: Pattern -> PatternType
getPatternType pattern = case pattern of -- Case instead of pattern match for readability
    WinP        -> WIN
    TieP        -> TIE
    LoseP       -> LOSE
    LikelyWP    -> LIKELYWIN
    LikelyLP    -> LIKELYLOSE

checkLWP :: Board -> Player -> Bool
checkLWP board player = if not (checkPattern board player WinP || checkPattern board player LoseP)
    then foldr (||) False [ countPos r player == 2 && countPos r N == 1 | r <- [ top board, middle board, bottom board, getDiag board LMR, getDiag board RML ] ]
    else False

checkPattern :: Board -> Player -> Pattern -> Bool
checkPattern board player pattern = case pattern of
    WinP        -> winWeight board player == winVal WIN
    LoseP       -> winWeight board player == winVal LOSE
    TieP        -> determineWin board == Just N
    LikelyWP    -> checkLWP board player
    LikelyLP    -> checkLWP board $ switchPlayer player

patternWeight :: Board -> Player -> Int
patternWeight board player = sum [ winVal $ getPatternType p | p <- patterns, checkPattern board player p ]

getTotalPatternWeight :: Board -> Player -> Player -> Int -> Int
-- Get total pattern weight looking n moves ahead, where the player moving in the given turn is move
getTotalPatternWeight board player move n = if n == 0 || checkGameOver board
    then patternWeight board player
    else sum [ getTotalPatternWeight b player (switchPlayer move) (n - 1) | b <- findAllMoves board move ]

winWeight :: Board -> Player -> Int
winWeight board player = winVal $ case determineWin board of
    Nothing     -> NEUTRAL -- Game is not over
    (Just N)    -> TIE -- Tie
    (Just p)    -> if p == player then WIN else LOSE -- Lost

countWins :: [Board] -> Player -> Int
countWins boards player = sum [ winWeight board player | board <- boards ]

-- winPatternsWeight :: Board -> Player -> Int

findAllMoves :: Board -> Player -> [Board]
findAllMoves board player = [ setByPosition board coords place | coords <- clist, getByPosition board coords == E ]
    where
        clist = zip [1, 1, 1, 2, 2, 2, 3, 3, 3] [1, 2, 3, 1, 2, 3, 1, 2, 3]
        place = getPlace player

getWinWeight :: Board -> Player -> Int
getWinWeight board player = countWins ( findAllMoves board player ) player

simulateOpponent :: Board -> Player -> [Board]
simulateOpponent board player = [ b | b <- findAllMoves board $ switchPlayer player ]

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
getTotalWinWeight board player = sum [ patternWeight b player | b <- findAllOutcomes board player ]
