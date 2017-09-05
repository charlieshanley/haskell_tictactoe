# haskell_tictactoe

This project implements a command-line tic-tac-toe game. The user plays against the computer.
Since the complete game tree is small enough to be computed, the computer plays perfectly.
Because of this, and because tic-tac-toe is a solved game (both players can guarantee a draw),
it is only possible for the player to draw or lose.


## How to run

`ghc --Make PlayTictactoe.hs` will produce an executable that you run to play.
Tictactoe.hs and TictactoeSolver.hs are modules imported by PlayTictactoe.hs.

## Notes on the implementation

The program generates a rose tree (tree with any number of branches at each node) of every
possible game state. A modified maximin algorithm determines which move the computer plays.

Maximin assigns a numeric score to terminal nodes of the game tree (1 if the computer wins,
-1 if the player wins, 0 for a draw), and propagates these scores upward by taking the maximum
or minimum score of the possible moves (that is, children of the current node in the game tree)
during the computer's and player's turns, respectively. Thus, it represents the best score that
the current player can guarantee. The maximin algorithm assumes that the opponent will play perfectly
and does not consider mistakes the opponent may make. In this form, maximin also does not care
when the end of the game is reached --a guaranteed win in one move is not distinguished from a
guaranteed win in several moves.

The modification made here is that scores are reduced toward zero each time they propagate from
a child node to the parent. The effect is that the computer player prefers an early win to a later win,
and contrariwise prefers a late loss to an earlier loss (or, it would if it could ever lose).
Draws, which happen when no open spaces remain on the board, do not vary in when they occur.
