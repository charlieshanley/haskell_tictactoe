import Tictactoe
import TictactoeSolver
import Data.Maybe (fromJust)
import Data.Char (isNumber)
import Control.Monad (when)

main :: IO ()
main = putStrLn intro >>
       playAI newGame

playAI :: GameState -> IO ()
playAI gs =
    (putStrLn . showBoard . board) gs >>
    case endgame . board $ gs of
        Just end -> putStrLn . announce $ end
        Nothing -> turn gs >>= playAI
    where turn = if whoseTurn gs == X then playerTurn else aiTurn

aiTurn :: GameState -> IO GameState
aiTurn gs = 
    putStrLn "My move:" >>
    return (flip unsafeMove gs . fromJust . decide hastyMaximin $ gs)


playerTurn ::  GameState -> IO GameState
playerTurn gs0 =
    putStrLn "Your move:" >>
    getLine        >>= \l ->
    maybe (tryAgain l gs0) return (maybeInt l >>= flip move gs0)

tryAgain :: String -> GameState -> IO GameState
tryAgain c gs =
    putStrLn (c ++ " is not a valid move. Pick again.") >>
    playerTurn gs

maybeInt :: String -> Maybe Int
maybeInt (c:[])
    | isNumber c = Just (read [c] :: Int)
maybeInt _       = Nothing

announce :: Endgame -> String
announce Draw  = "It's a draw!"
announce OWins = "I have won! Good game."
announce XWins = "You won? But how?!"

showBoard :: Board -> String
showBoard = unlines . map unwords . (map . map) show


intro =
    "\nLet's play Tic-tac-toe.\nYou go first. You're X; I'm O.\n" ++
    "Enter a number to mark a space.\n"
