module TictactoeSolver (decide) where

import Tictactoe
import Data.List
import Data.Maybe (fromJust)


data Rose a = Rose a [Rose a] deriving (Show, Eq)

instance Functor Rose where
    f `fmap` (Rose x ys) = Rose (f x) ((map . fmap) f ys)


-- The main thing
decide :: Board -> Maybe Int
decide = pickMove . lookAhead O


-------------------------------------------------------------------------------
-- Generate tree of possible board states

lookAhead :: Mark -> Board -> Rose Board
lookAhead m b
    | endgame b /= Nothing = Rose b []
    | otherwise            = Rose b $ lookAhead (succ m) `map` allMoves m b


allMoves :: Mark -> Board -> [Board]
allMoves m b = moveThere `map` legalMoves
    where moveThere = \i -> unsafeMove m i b
          legalMoves = emptyInds b

emptyInds :: Board -> [Int]
emptyInds = map getInd . filter ((==Empty) . getMark) . concat


-------------------------------------------------------------------------------
-- Score board states and select a good move

pickMove :: Rose Board -> Maybe Int
pickMove (Rose b0 rbs)= unlist $ b1 `boardDiff` b0
    where assess = minimax . fmap (score . endgame)
          best   = maximum . map assess $ rbs
          Just (Rose b1 _) = find ((==best) . assess) rbs


boardDiff :: Board -> Board -> [Int]
boardDiff b1 b0 = f b1 \\ f b0
    where f = map getInd . filter ((/=Empty) . getMark) . concat

unlist :: [a] -> Maybe a
unlist [x] = Just x
unlist _   = Nothing

-- Outermost Rose is a move that the comp can take; next are those its opponent can take

maximin (Rose n []) = n
maximin (Rose _ rs) = maximum . map minimax $ rs

minimax (Rose n []) = n
minimax (Rose _ rs) = minimum . map maximin $ rs

-- Doesn't work. Unsure why.
negamax :: (Num c, Ord c) => Rose c -> c
negamax (Rose n []) = n
negamax (Rose _ rs) = maximum . map (negate . negamax) $ rs


score :: Maybe Endgame -> Int
score (Just XWins) = negate 1
score (Just OWins) = 1
score _            = 0



-------------------------------------------------------------------------------
-- testing

draw = (return newBoard) >>=
    move X 1 >>= move O 5 >>= move X 3 >>= move O 2 >>= move X 8
iwin = (return newBoard) >>=
    move X 1 >>= move O 7 >>= move X 4 >>= move O 9 >>= move X 2
youwin = (return newBoard) >>=
    move X 1 >>= move O 4 >>= move X 9 >>= move O 8 >>= move X 3


testTree :: Board -> Int
testTree = negamax . fmap (score . endgame) . lookAhead O

testTree' = maximin . fmap (score . endgame) . lookAhead O

showB :: Maybe Board -> IO ()
showB = putStr . unlines . map unwords . (map . map) show . fromJust