module Tictactoe
    (
    Mark(..),
    Board,
    Endgame,
    newBoard,
    move,
    unsafeMove,
    endgame
    ) where

import Data.List
import Data.Maybe
import GHC.Exts (groupWith)


data Mark = X | O | Empty deriving (Eq, Show, Read)
data Space = Space { getMark :: Mark, getInd :: Int }
instance Show Space where
    show (Space X _)     = "X"
    show (Space O _)     = "O"
    show (Space Empty n) = show n
type Board = [[Space]]
data Endgame = Draw | OWins | XWins deriving (Eq, Show, Read)


newBoard :: Board
newBoard = (map . map) (Space Empty) [[1..3], [4..6], [7..9]]

-------------------------------------------------------------------------------
-- Test for endgame

endgame :: Board -> Maybe Endgame
endgame b
    | elem [O, O, O] . getWinLines $ b         = Just OWins
    | elem [X, X, X] . getWinLines $ b         = Just XWins
    | notElem Empty . map getMark . concat $ b = Just Draw
    | otherwise                                = Nothing

getWinLines :: Board -> [[Mark]]
getWinLines b = (map . map) getMark spaceLines
    where spaceLines = diagonal b : (diagonal . reverse $ b) : transpose b ++ b

diagonal :: [[a]] -> [a]
diagonal ((x:_):rs) = x : diagonal (map tail rs)
diagonal _          = []

-------------------------------------------------------------------------------
-- Make moves on the board

move :: Mark -> Int -> Board -> Maybe Board
move m i b = sqUnconcat <$> maybeModifyEl (makeMark m) i (concat b)

unsafeMove :: Mark -> Int -> Board -> Board
unsafeMove m i = fromJust . move m i

maybeModifyEl :: (a -> Maybe a) -> Int -> [a] -> Maybe [a]
maybeModifyEl f i as = (xs ++) . (:ys) <$> f y
    where (xs, y:ys) = splitAt (i - 1) as

sqUnconcat :: [Space] -> Board
sqUnconcat xs = groupWith ((`div` dim) . (subtract 1) . getInd) xs
    where dim = ceiling . sqrt . fromIntegral . length $ xs

makeMark :: Mark -> Space -> Maybe Space
makeMark new (Space Empty n) = Just (Space new n)
makeMark _   _               = Nothing