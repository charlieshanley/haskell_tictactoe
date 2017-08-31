module TictactoeSolver (decide) where

import Tictactoe
import Data.List
import Data.Maybe (fromJust)


data Rose a = Rose a [Rose a] deriving (Show, Eq)

instance Functor Rose where
    f `fmap` (Rose x ys) = Rose (f x) $ (map . fmap) f ys

data Assessment = Assessment {gameState :: GameState, merit :: Int}

-- The main thing
decide :: GameState -> Maybe Int
decide = pickMove . lookAhead


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
-- Score board states and select a good move

pickMove :: Rose GameState -> Maybe Int
pickMove (Rose _ []) = Nothing
pickMove r           = lastMove <$> nextGameState
    where (Rose current candidates) = maximin r
          nextGameState = gameState . unrose <$> find isBest candidates
          isBest = (==(merit current)) . merit . unrose

maximin :: Rose GameState -> Rose Assessment
maximin (Rose gs []) = Rose (Assessment gs (scoreGS gs)) []
maximin (Rose gs rs) = Rose (Assessment gs (review children)) children
    where children = map maximin rs
          review = superlative . map (merit . unrose)
          superlative = if whoseTurn gs == O then maximum else minimum

scoreGS :: GameState -> Int
scoreGS = score . endgame . board

score :: Maybe Endgame -> Int
score (Just XWins) = negate 1
score (Just OWins) = 1
score _            = 0

unrose :: Rose a -> a
unrose (Rose x _) = x



showGameTree :: Rose GameState -> String
showGameTree rg = show' 0 rg
    where show' i (Rose g rs) = intercalate "\n" $ showGame i g : map (show' (i+2)) rs

showGame :: Int -> GameState -> String
showGame i = unlines . map (indent . unwords) . (map . map) show . board
    where indent s = replicate i ' ' ++ s

-------------------------------------------------------------------------------
-- testing

draw = (return newGame) >>= move 1 >>= move 5 >>= move 3 >>= move 2 >>= move 8
iwin = (return newGame) >>= move 1 >>= move 7 >>= move 4 >>= move 9 >>= move 2
youwin = (return newGame) >>= move 1 >>= move 4 >>= move 9 >>= move 8 >>= move 3
