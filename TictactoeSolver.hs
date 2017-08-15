import Tictactoe
import Data.Tree

main = undefined



intro = "\nI am TicTacToe9000. Welcome to my Tic-tac-Temple. Let's play.\n" ++
        "You go first. You're X; I'm O. Enter a number to mark a space.\n\n"


lookAhead :: Board -> Tree Board
lookAhead brd = Tree brd (map lookAhead $ allMoves )


allMoves :: Mark -> Board -> [Board]
allMoves m b = moveI `map` legalMoves
    where legalMoves = emptyInds b
          moveI = \i -> unsafeMove m i b

emptyInds :: Board -> [Int]
emptyInds = map getInd . filter ((==Empty) . getMark) . concat



-- type GameState = Tree (Int, Board)


-- showBoard :: Board -> String
-- showBoard = unlines . map unwords . (map . map) show

-- -- play :: Board -> Board
