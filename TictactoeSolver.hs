import Tictactoe


data Rose a = Rose a [Rose a] deriving (Show, Eq)

lookAhead :: Mark -> Board -> Rose Board
lookAhead m b = Rose b (nextTurn `map` allMoves m b)
    where nextTurn = lookAhead (succ m)


allMoves :: Mark -> Board -> [Board]
allMoves m b = moveI `map` (emptyInds b)
    where moveI = \i -> unsafeMove m i b

emptyInds :: Board -> [Int]
emptyInds = map getInd . filter ((==Empty) . getMark) . concat




-- showBoard :: Board -> String
-- showBoard = unlines . map unwords . (map . map) show

-- -- play :: Board -> Board


main = undefined



intro = "\nI am TicTacToe9000. Welcome to my Tic-tac-Temple. Let's play.\n" ++
        "You go first. You're X; I'm O. Enter a number to mark a space.\n\n"