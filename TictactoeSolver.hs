import Tictactoe


data Rose a = Rose a [Rose a] deriving (Show, Eq)

instance Functor Rose where
    f `fmap` (Rose x ys) = Rose (f x) ((map . fmap) f ys)


-------------------------------------------------------------------------------
-- Generate tree of possible board states

lookAhead :: Mark -> Board -> Rose Board
lookAhead m b = Rose b (nextTurn `map` allMoves m b)
    where nextTurn = lookAhead (succ m)


allMoves :: Mark -> Board -> [Board]
allMoves m b = moveI `map` (emptyInds b)
    where moveI = \i -> unsafeMove m i b

emptyInds :: Board -> [Int]
emptyInds = map getInd . filter ((==Empty) . getMark) . concat


-------------------------------------------------------------------------------
-- Score board states and select a good move

prognosis :: Rose Board -> Rose Double
prognosis (Rose x ys) = undefined

maximin :: (Num a, Ord a) => Rose a -> a
maximin (Rose x []) = x
maximin (Rose x ys) = maximum $ map (negate . maximin) ys

score :: Maybe Endgame -> Int
score (Just XWins) = negate 1
score (Just OWins) = 1
score _            = 0

-- showBoard :: Board -> String
-- showBoard = unlines . map unwords . (map . map) show

-- -- play :: Board -> Board



-- intro = "\nI am TicTacToe9000. Welcome to my Tic-tac-Temple. Let's play.\n" ++
--         "You go first. You're X; I'm O. Enter a number to mark a space.\n\n"





-- Oh, compiled it's plenty fast. It's only too slow interpreted in GHCi
-- main = putStrLn . show $ lookAhead O <$> almost

-- almost = (return newBoard) >>= move X 1