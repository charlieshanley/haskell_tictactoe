import Tictactoe
import TictactoeSolver
import Data.Maybe (fromJust)
import Data.Char (isNumber)

main :: IO ()
main = undefined

playAI :: GameState -> IO ()
playAI gs =
    case endgame . board $ gs of
        Just end -> putStrLn . announce $ end
        Nothing -> turn gs >>= playAI
    where turn = if whoseTurn gs == X then playerTurn else return . aiTurn


playPVP :: GameState -> IO ()
playPVP = undefined


aiTurn :: GameState -> GameState
aiTurn gs = flip unsafeMove gs . fromJust . decide hastyMaximin $ gs


playerTurn ::  GameState -> IO GameState
-- playerTurn = undefined
playerTurn gs0 = getChar >>= (\c ->
    maybeInt c >>= flip move gs0 >>= \t ->
        case t of
            Just gs1 -> return gs1
            Nothing -> tryAgain gs0
    )


tryAgain :: GameState -> IO GameState
tryAgain gs = putStrLn "That is not a valid move. Pick again." >> playerTurn gs

maybeInt :: Char -> Maybe Int
maybeInt c
    | isNumber c = Just (read [c] :: Int)
    | otherwise  = Nothing

announce :: Endgame -> String
announce Draw  = "It's a draw!"
announce OWins = "I have won! Good game."
announce XWins = "You won? But how?!"

showBoard :: Board -> String
showBoard = unlines . map unwords . (map . map) show


intro = "\nI am TicTacToe9000. Welcome to my Tic-tac-Temple. Let's play.\n" ++
        "You go first. You're X; I'm O. Enter a number to mark a space.\n"
