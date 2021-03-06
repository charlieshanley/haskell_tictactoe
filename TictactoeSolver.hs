module TictactoeSolver
    ( decide
    , maximin
    , hastyMaximin
    ) where

import Data.Maybe(isNothing)
import Tictactoe
import Data.List

data Rose a = Rose a [Rose a] deriving (Show, Eq)

instance Functor Rose where
    f `fmap` (Rose x ys) = Rose (f x) $ (map . fmap) f ys

data Assessment = Assessment {gameState :: GameState, merit :: Float}

-- The main thing
decide :: (Rose GameState -> Rose Assessment) -> GameState -> Maybe Int
decide evaluator = pickMove . evaluator . lookAhead

-------------------------------------------------------------------------------
-- Generate tree of possible board states

lookAhead :: GameState -> Rose GameState
lookAhead gs@(GameState _ _ b)
    | isNothing(endgame b) = Rose gs $ lookAhead `map` allMoves gs
    | otherwise            = Rose gs []


allMoves :: GameState -> [GameState]
allMoves gs@(GameState _ _ b) = moveThere `map` legalMoves
    where moveThere = (`unsafeMove` gs)
          legalMoves = emptyInds b

emptyInds :: Board -> [Int]
emptyInds = map getInd . filter ((==Empty) . getMark) . concat

-------------------------------------------------------------------------------
-- Algorithms to evaluate the tree of GameStates

-- Assumes perfect oponent; does not care how long game takes.
maximin :: Rose GameState -> Rose Assessment
maximin (Rose gs []) = Rose (Assessment gs (scoreGS gs)) []
maximin (Rose gs rs) = Rose (Assessment gs (review children)) children
    where children = map maximin rs
          review = superlative . map (merit . unrose)
          superlative = if whoseTurn gs == O then maximum else minimum

-- Assumes perfect oponent; prefers to win quickly and lose slowly.
hastyMaximin :: Rose GameState -> Rose Assessment
hastyMaximin (Rose gs []) = Rose (Assessment gs (scoreGS gs)) []
hastyMaximin (Rose gs rs) = Rose (Assessment gs (review children)) children
    where children = map hastyMaximin rs
          review = (0.5 *) . superlative . map (merit . unrose)
          superlative = if whoseTurn gs == O then maximum else minimum


scoreGS :: (Num a) => GameState -> a
scoreGS = score . endgame . board

score :: (Num a) => Maybe Endgame -> a
score (Just XWins) = negate 1
score (Just OWins) = 1
score _            = 0

unrose :: Rose a -> a
unrose (Rose x _) = x


-------------------------------------------------------------------------------
-- Pick the best move

pickMove :: Rose Assessment -> Maybe Int
pickMove (Rose _ []) = Nothing
pickMove (Rose _ potential) = lastMove <$> chosen
    where candidates = unrose `map` potential
          bestMerit = maximum . map merit $ candidates
          chosen = gameState <$> find ((bestMerit==) . merit) candidates

-------------------------------------------------------------------------------
-- development utilities

showTree :: (Int -> a -> String) -> Rose a -> String
showTree showFun = show' 0
    where show' i (Rose x xs) = intercalate "\n" $ showFun i x : map (show' (i+4)) xs

showIndent :: (Show s) => Int -> s -> String
showIndent indent = (replicate indent ' ' ++) . show

showGame :: Int -> GameState -> String
showGame i = unlines . map (indent . unwords . map show) . board
    where indent s = replicate i ' ' ++ s