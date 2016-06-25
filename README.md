# Tic Tac Toe AI
This is a project to develop a working Tic Tac Toe AI.  I will start with a very simple version and increase its complexity over time.  It is written in Haskell.

## Current
Currently, the AI looks at each possible moves and computes the total win weight of each, choosing the move with the greatest win weight.  This is overridden if a possible move will win, in which case it makes that move.

The win weight is the total of all possible win, tie and lose outcomes for the game continuing from the result of a given move.  Win, Tie and Lose are now associated with the WinP, TieP and LoseP Patterns in the Algo module.

## Next
The next step will be to have an AI which, instead of calculating all possible games, looks ahead a few steps for patterns indicative of likely wins.  With this version, the difficulty can be varied by setting the number of moves it looks ahead, and by setting how many patterns it looks for.

## Overview

As this project is for the purposes of learning, I will not be doing any research on implementing AIs.  I will be implementing each step, possibly going as far as techniques like machine learning, from scratch and without any guidance.

The current work is to implement the very basics, an algorithm which will evaluate all possible endings (win/lose/tie) resulting from each possible move and make the move resulting in the highest possible combination of wins (weighted 2) and ties (weighted 1).  I expect this to be very slow and not terribly effective.

The next step will be to work out a way to guess the likelihood of winning based on a given board (which I will probably do at first by manually looking through the possible-ending data for any early patterns), at which point I can speed up (but presumably worsen) the AI by only looking a few moves ahead.  This will also allow me to set difficulty levels by the number of moves it considers in advance.

After that, I will attempt to set it up such that it can use past data to determine the likelihood of a board to win more accurately.  Initially I will probably set it up to look for specific boards, not more general patterns, for a sort of tree of winning possibilities.

The final step is to set it up to look for more general patterns (say, two in a line and a third open spot), if I can actually figure out how to do that without using anyone else's research.
