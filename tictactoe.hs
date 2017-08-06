import Data.List

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

type Board = [[Space]]

data Endgame = Draw | IWin | YouWin


intro = "\nI am TicTacToe9000. Welcome to my Tic-tac-Temple. Let's play.\n" ++
        "You go first. You're X; I'm O. Enter a number to mark a space.\n\n"

newBoard :: Board
newBoard = (map . map) (Space Empty) [[1..3], [4..6], [7..9]]

endgame :: Board -> Maybe Endgame
endgame board
    | elem [O, O, O] . getWinLines $ board         = Just IWin
    | elem [X, X, X] . getWinLines $ board         = Just YouWin
    | notElem Empty . map getMark . toList $ board = Just Draw
    | otherwise                                    = Nothing

getWinLines :: Board -> [[Mark]]
getWinLines b = (map . map) getMark spaceLines
    where spaceLines = (diagonal b) : (diagonal . reverse $ b) : (transpose b) ++ b

diagonal :: [[a]] -> [a]
diagonal [] = []
diagonal [[]] = []
diagonal ((x:_):rs) = x : (diagonal $ map tail rs)


makeMark :: Mark -> Space -> Maybe Space
makeMark new (Space Empty n) = Just (Space new n)
makeMark _ (Space X _) = Nothing
makeMark _ (Space O _) = Nothing

move :: Board -> Player -> Int -> Maybe Board
move b Human i = 
move b Computer i = 
    where
        inds = (map . map) getInd newBoard
        row = findIndex (elem i) inds
        col =  row >>= (\r -> findIndex ( == 1) $ inds !! r)




-- play :: Board -> Board

-- lookAhead :: Board -> Tree Board
