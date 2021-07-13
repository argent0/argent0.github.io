---
title: First sixty days of Haskell.
categories: "haskell"
order: 1
abstract: "Write abstract"
date: 2014-12-10
---

In a nutshell sudoku puzzle consists on a $9 \times 9$ grid of cells. Some cells contain
numbers between 1 and 9 but most of them are empty. The player's mission, shall
he choose to accept it, is to complete the empty cells with numbers between 1
and 9 such that: Every row contains every digit; same for columns and $3 \times
3$ box.  A complete description of the Sudoku puzzle can be found
[here](www.wikipedia.org).

To represent the progress of the game I will use _moves_. A move is writing a
digit into a cell. For example: write "1" in the column "2" of row "3". Here is
the haskell definition:

```haskell
data Move = Move
  { moveRow :: Int
  , moveCol :: Int
  , moveNumer :: Int } deriving Show
```

So `Move 1 2 3` represents: "in the square at column 2 and row 1 write 3".

An empty sudoku, one with all of its cells empty, is a sudoku where all moves
are possible. Filling one of the cells eliminates several possible moves. For
example, writing a "1" in a row eliminates the possibility of writing a "1"
in other cells of the same row.

So instead of representing the state of the puzzle by the moves I've already
made. I'll represent it by all possible remaining moves.

```haskell
base :: Int
base = 3

size :: Int
size = base*base --*

allMoves :: [Move]
allMoves = Move <$> [1..size] <*> [1..size] <*> [1..size]
```

Using this representation the solved sudoku is one where there no more moves
left to perform. However, having no more legals moves to perform doesn't mean
that there are not empty cells left.  Solving the puzzle is extracting moves
from the list in such a way that when there are no more moves left all the cells
are filled.

Performing a move means taking a move from the current state and eliminating all
those moves that have are now illegal.

```haskell
makeMove :: Move -> [Move]  -> [Move]
makeMove m =
  enforceRowRule m . enforceColRule m . enforceAreaRule m . enforceOneValuePerCell m
```

Each rule can be enforced in its own function for example this is the function
for "only one value per cell:"

```haskell
enforceOneValuePerCell :: Move -> [Move] -> [Move]
enforceOneValuePerCell (Move cx cy _) = 
  filter select 
  where
  select (Move x y _) = not (x==cx && y==cy)
```

Let's see how this works for the case of performing `Move 1 1 1`:

```terminal
*Main> take 2 allMoves 
[Move {moveRow = 1, moveCol = 1, moveNumer = 1},Move {moveRow = 1, moveCol = 1, moveNumer = 2}]
*Main> take 2 $ makeMove (Move 1 1 1) allMoves 
[Move {moveRow = 1, moveCol = 2, moveNumer = 2},Move {moveRow = 1, moveCol = 2, moveNumer = 3}]
```

There is still the issue of whether the move to be performed is a valid move in
the current state of the puzzle. But it is easy to create a function that
performs that check before performing the move:

```haskell
secure :: (Move -> [Move] -> [Move]) -> Move -> [Move] -> Maybe [Move]
secure rule move state
  | move `elem` state = Just $ rule move state
  | otherwise = Nothing
```

Here is the result of attempting an invalid move(write two "1" next to each
other):

```terminal
*Main> secure makeMove (Move 1 1 1) allMoves >>= secure makeMove (Move 1 2 1)
Nothing
```

<!--These lists of moves, represent all possible future completions of the sudoku
puzzle that don't break any rule.

The full set of constrains is just a *composition* of constrains:


So, given a set of moves $S_0$, there are *arrows* $r_m$ where $m \in S_0$:

$$ r_m: S_0  \longrightarrow  \big\{ x \in S_0: x \text{ is a legal move after performing $m$ and applying the rule $r$ } \big\} $$

The notation $r_m$ means make the move $m$ and apply the rule $r$.

These arrows are composable:

$$ r_m \circ q_n': S_0 \longrightarrow \big\{ x \in S_0: x \text{ ... } \big\} $$

and composition is associative:

$$ r_m \circ q_n \circ s_l =r_m \circ (q_n \circ s_l) = (r_m \circ q_n) \circ s_l $$

one could also think of a special $r_0$ that makes no move, so it's an identity.

This means that the sets of moves and the functions enforcing the rules form a
*Category*.

The function `makeMove m` is a combination of $r_m$'s:

$$
R_m: S_0 \longrightarrow \big\{S_0-m \big\} = r_m \circ q_m \circ s_m \circ t_m
$$

where $S_0-m$ means the set of moves after making move $m$ in a sudoku. By
restricting the arrows of the previous category to arrows of the form $R_m$ and
$r_0$ we obtain new a category where the objects are sets of possible moves from:
a solved sudoku, partially solved sudoku(where no errors have been made) or a
sudoku that can't be solved.

This way, solving the sudoku means: starting from a given object  search for one
instance of a particular kind of object in this category(the solved sudoku case
above). This can be done in the same way as a
[graph search](http://en.wikipedia.org/wiki/Depth-first_search) is done.

The following `solve` function is *depth first search* (DFS) that stops when it finds a
solved sudoku or when all options are exhausted. Its main difference with
graph's DFS is that it tries the nodes in ascending order of number of options.
In other words, starting with where only one number can be written, then two,
etc... -->

Now onto how to obtain a solution. The basic idea behind this solution is depth
first search [DFS](http://en.wikipedia.org/wiki/Depth-first_search). DFS is a
way searching a graph where you go through the edges a deep as deep as you can
and then backtrack to try other paths. In terms of solving the sudoku puzzle,
this means completing the as many cells as you can before running out of
possible moves.

If there are no more moves and all cells have a digit then the puzzle is solved.

There are two customizations to be made to vanilla DFS for this puzzle. The
first is that, when choosing the cell to fill next, it will choose the cell with
possible completions available. This allows the algorithm to find dead ends
sooner. The second customization is that it will start backtracking the moment
there is at least a cell with no possible completions.

```haskell
solve :: [Move] -> Maybe [Move]
solve moves =
  makeMoves moves >>= solution
  where
  solution = go moves (size*size-length moves) 
  go mvs 0 [] = Just mvs 
  go _ _ [] = Nothing 
  go mvs missing (m:ms) =
    if solvable
      then case next of
        Just n -> Just n
        Nothing ->
        go mvs missing (sortByOptions ms)
      else
        Nothing
    where
    next = go (m:mvs) (missing-1) (sortByOptions (makeMove m ms))
    solvable = all hasOptions allPositions
    hasOptions (a,b) =
      or $ (\(Move c d _) -> (a==c) && (b==d)) <$> (m:mvs ++ ms)
    allPositions = (,) <$> [1..size] <*> [1..size]
```

The `solve` function returns the list of moves that solve the puzzle when there
is such a list. Its argument is a list of all the moves provided as clues.

# Final remarks

Representing a sudoku puzzle by all the available moves (in a list `[Move]`).
Led to represent the application of "sudoku laws" as functions `[Move] ->
[Move]` labeled by a selected move.

This, in turn, showed how the possible solutions could be seen as nodes in a
graph where the edges relating two nodes is "performing a move" on the source
node.

Once this interpretation is clear, searching for the solution using a DFS seems
natural.

I consider now that the essence of the problem is graph search, but I find the
consequences of the choose of the representation of the nodes (`[Move]` in this
case) rather interesting.

In my code, I ask for a list of moves that leads to a solved sudoku.
The intermediate results, the nodes, also contain the possible moves onwards.
I considered representing this intermediate states as $9 \times 9$ matrices. That would
have been grounded on the fact that the end result, after I find all the moves,
is constructing such a representation.

Transforming from the `[Move]` representation to a $9 \times 9$ matrix representation
is easier than transforming from $9 \times 9$ matrix to `[Move]`. Even more so, when
you only do it at once at the end.

Using the $9 \times 9$ would have implied, for every step of DFS, to make the
transformation to `[Move]`, choose a move and then move to the next node 
rebuilding a $9 \times 9$ representation by adding a number in the correct cell.

Knowing how thinks work out using `[Move]`, it is clear that this representation
switching is unnecessary and consist on undoing what was just done before. But
if I wouldn't have known about `[Move]` then I would probably not have noticed
the problem.

# Appendix I

To understand the `allMoves` definition you can use `GHCi` to inspect the types:

```terminal
*Main> :t  (<$>)
(<$>) :: Functor f => (a -> b) -> f a -> f b
*Main> :t  (<*>)
(<*>) :: Applicative f => f (a -> b) -> f a -> f b
*Main> :t Move
Move :: Int -> Int -> Int -> Move
*Main> :t (Move <$> [1..size])
(Move <$> [1..size]) :: [Int -> Int -> Move]
*Main> :t (Move <$> [1..size] <*> [1..size]) 
(Move <$> [1..size] <*> [1..size]) :: [Int -> Move]
*Main> :t (Move <$> [1..size] <*> [1..size] <*> [1..size]) 
(Move <$> [1..size] <*> [1..size] <*> [1..size]) :: [Move]
*Main> 
```

<script type="text/javascript" async
  src="https://cdnjs.cloudflare.com/ajax/libs/mathjax/2.7.7/MathJax.js?config=TeX-MML-AM_CHTML">
</script>

<script type="text/x-mathjax-config">
  MathJax.Hub.Config({tex2jax: {inlineMath: [['$','$'], ['\\(','\\)']]}});
</script>
