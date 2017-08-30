import Tictactoe
import TictactoeSolver


main = undefined

showBoard :: Board -> String
showBoard = unlines . map unwords . (map . map) show

-- -- play :: Board -> Board



intro = "\nI am TicTacToe9000. Welcome to my Tic-tac-Temple. Let's play.\n" ++
        "You go first. You're X; I'm O. Enter a number to mark a space.\n"
