import Data.List
import Data.Maybe
import Data.Tree

-- main = do
--     putStr intro
--     play newBoard

-------------------------------------------------------------------------------
-- Data types to represent our scenario
data Mark = X | O | Empty deriving (Eq, Show, Read)
data Space = Space { getMark :: Mark, getInd :: Int }
instance Show Space where
    show (Space X _)     = "X"
    show (Space O _)     = "O"
    show (Space Empty n) = show n
data Player = Human | Computer deriving (Eq, Show, Read)
type Board = [[Space]]
data Endgame = Draw | IWin | YouWin deriving (Eq, Show, Read)

-------------------------------------------------------------------------------
-- Some constants
intro = "\nI am TicTacToe9000. Welcome to my Tic-tac-Temple. Let's play.\n" ++
        "You go first. You're X; I'm O. Enter a number to mark a space.\n\n"

newBoard :: Board
newBoard = (map . map) (Space Empty) [[1..3], [4..6], [7..9]]

-------------------------------------------------------------------------------
-- Test for endgame
endgame :: Board -> Maybe Endgame
endgame b
    | elem [O, O, O] . getWinLines $ b         = Just IWin
    | elem [X, X, X] . getWinLines $ b         = Just YouWin
    | notElem Empty . map getMark . concat $ b = Just Draw
    | otherwise                                = Nothing

getWinLines :: Board -> [[Mark]]
getWinLines b = (map . map) getMark spaceLines
    where spaceLines = (diagonal b) : (diagonal . reverse $ b) : (transpose b) ++ b

diagonal :: [[a]] -> [a]
diagonal ((x:_):rs) = x : (diagonal $ map tail rs)
diagonal _          = []

-------------------------------------------------------------------------------
-- Make moves on the board
makeMark :: Mark -> Space -> Maybe Space
makeMark new (Space Empty n) = Just (Space new n)
makeMark _ (Space X _) = Nothing
makeMark _ (Space O _) = Nothing

maybeChangeElement :: (a -> Maybe a) -> [[a]] -> Int -> Int -> Maybe [[a]]
maybeChangeElement f orig r c = place beforeRs afterRs <$> newR
    where (beforeRs, (oldR:afterRs)) = splitAt r orig
          (beforeVs, (oldV:afterVs)) = splitAt c oldR
          newR = place beforeVs afterVs <$> newV
          newV = f oldV
          place before after = (before ++) . (++ after) . (:[])

markSpace :: Mark -> Maybe Int -> Maybe Int -> Board -> Maybe Board
markSpace m row col b = do
    r <- row
    c <- col
    maybeChangeElement (makeMark m) b r c

move :: Mark -> Int -> Board -> Maybe Board
move m i b = markSpace m row col b
    where inds = (map . map) getInd b
          row = findIndex (elem i) inds
          col =  row >>= findIndex (== i) . (!!) inds

unsafeMove :: Mark -> Int -> Board -> Board
unsafeMove m i b = fromJust $ move m i b

-------------------------------------------------------------------------------
-- Decide which move to make
emptyInds :: Board -> [Int]
emptyInds = map getInd . filter ((==Empty) . getMark) . concat

allMoves :: Mark -> Board -> [Board]
allMoves m b = moveI `map` legalMoves
    where legalMoves = emptyInds b
          moveI = \i -> unsafeMove m i b

-- lookAhead :: Board -> Tree (Maybe Board)
-- lookAhead brd = Tree brd ()
--     |

-- type GameState = Tree (Int, Board)


-- showBoard :: Board -> String
-- showBoard = unlines . map unwords . (map . map) show

-- -- play :: Board -> Board
