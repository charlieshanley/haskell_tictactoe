import Tictactoe
import TictactoeSolver
import Data.Maybe (fromJust)
import Data.Char (isNumber)

main :: IO ()
main = putStrLn intro >>
       playAI newGame

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
playerTurn gs0 =
    (putStrLn . showBoard . board) gs0  >>
    getChar                      >>= \c ->
    maybe (tryAgain gs0) return (maybeInt c >>= flip move gs0)

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


intro =
    "\nLet's play Tic-tac-toe.\nYou go first. You're X; I'm O.\n" ++
    "Enter a number to mark a space.\n"
