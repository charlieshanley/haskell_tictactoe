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

pickMove :: Rose Board -> Int
pickMove (Rose b0 rbs)= undefined
    where prognoses = map (negamax . fmap (score . endgame)) rbs


-- Outermost Rose is a move that the comp can take; next are those its opponent can take
negamax :: (Num c, Ord c) => Rose c -> c
negamax (Rose n []) = n
negamax (Rose n rs) = minimum . map (negate . negamax) $ rs


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



-- showBoard :: Board -> String
-- showBoard = unlines . map unwords . (map . map) show

-- -- play :: Board -> Board



-- intro = "\nI am TicTacToe9000. Welcome to my Tic-tac-Temple. Let's play.\n" ++
--         "You go first. You're X; I'm O. Enter a number to mark a space.\n\n"





-- Oh, compiled it's plenty fast. It's only too slow interpreted in GHCi
-- main = putStrLn . show $ lookAhead O <$> almost

-- almost = (return newBoard) >>= move X 1