import           Data.Matrix
import qualified Data.Vector as V


-- main = do
--     putStr intro
--     play newBoard



data Mark = X | O | Empty deriving (Eq, Show, Read)

data Space = Space { getMark :: Mark, getInd :: Int }

instance Show Space where
    show (Space X _)     = "X"
    show (Space O _)     = "O"
    show (Space Empty n) = show n

data Player = Human | Computer deriving (Eq, Show, Read)

type Board = Matrix Space

data Endgame = Draw | IWin | YouWin


intro :: String
intro = "\nI am TicTacToe9000. Welcome to my Tic-tac-Temple. Let's play.\n" ++
        "You go first. You're X; I'm O. Enter a number to mark a space.\n\n"

newBoard :: Board
newBoard = fromList 3 3 . map (Space Empty) $ [1..9]

endgame :: Board -> Maybe Endgame
endgame board
    | elem [O, O, O] . getWinLines $ board         = Just IWin
    | elem [X, X, X] . getWinLines $ board         = Just YouWin
    | notElem Empty . map getMark . toList $ board = Just Draw
    | otherwise                                    = Nothing

getWinLines :: Board -> [[Mark]]
getWinLines brd = ($ brd) `map` funlist
    where funlist = (.) <$> toLists <$> [transpose, id]

-- move :: Board -> Player -> Int -> Board
-- move board player ind =



-- play :: Board -> Board

-- lookAhead :: Board -> Tree Board
