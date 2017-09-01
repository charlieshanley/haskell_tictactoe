module TictactoeSolver
    ( decide
    , maximin
    ) where

import Tictactoe
import Data.List
import Data.Maybe (fromJust)


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
    | endgame b == Nothing = Rose gs $ lookAhead `map` allMoves gs
    | otherwise            = Rose gs []


allMoves :: GameState -> [GameState]
allMoves gs@(GameState _ m b) = moveThere `map` legalMoves
    where moveThere = \i -> unsafeMove i gs
          legalMoves = emptyInds b

emptyInds :: Board -> [Int]
emptyInds = map getInd . filter ((==Empty) . getMark) . concat

-------------------------------------------------------------------------------
-- Evaluate the tree of GameStates

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
pickMove (Rose current potential) = lastMove <$> chosen
    where candidates = map unrose potential
          bestMerit = maximum . map merit $ candidates
          chosen = gameState <$> find ((bestMerit==) . merit) candidates

-------------------------------------------------------------------------------
-- development utilities

showTree :: (Int -> a -> String) -> Rose a -> String
showTree showFun r = show' 0 r
    where show' i (Rose x xs) = intercalate("\n") $ showFun i x : map (show' (i+4)) xs

showIndent :: (Show s) => Int -> s -> String
showIndent indent = (replicate indent ' ' ++) . show

showGame :: Int -> GameState -> String
showGame i = unlines . map (indent . unwords) . (map . map) show . board
    where indent s = replicate i ' ' ++ s

draw = (return newGame) >>= move 1 >>= move 5 >>= move 3 >>= move 2 >>= move 8
iwin = (return newGame) >>= move 1 >>= move 7 >>= move 4 >>= move 9 >>= move 2
youwin = (return newGame) >>= move 1 >>= move 4 >>= move 9 >>= move 8 >>= move 3
