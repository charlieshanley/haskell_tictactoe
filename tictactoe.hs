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

-------------------------------------------------------------------------------
-- Data types to represent our scenario
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
makeMark :: Mark -> Space -> Maybe Space
makeMark new (Space Empty n) = Just (Space new n)
makeMark _ (Space X _) = Nothing
makeMark _ (Space O _) = Nothing

-- Can we change Space to association list of Marks, so that we can concat
-- the board, lookup element, change it, and unconcat? Then we wouldn't have
-- to bother with this complicated stuff.

maybeChangeElement :: (a -> Maybe a) -> [[a]] -> Int -> Int -> Maybe [[a]]
maybeChangeElement f orig r c = place beforeRs afterRs <$> newR
    where (beforeRs, oldR:afterRs) = splitAt r orig
          (beforeVs, oldV:afterVs) = splitAt c oldR
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
          col =  row >>= elemIndex i . (!!) inds

unsafeMove :: Mark -> Int -> Board -> Board
unsafeMove m i = fromJust . move m i
