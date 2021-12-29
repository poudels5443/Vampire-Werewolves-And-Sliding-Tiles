Here three search methods (BFS, DFS, and HFS) are implemented to find the solution path for
vampire werewolves , and sliding-tile puzzle.

Vampire werewolves puzzle is represented as vamp-wolves in the code.
Sliding-tile puzzle is represented as sliding-tile in the code.

State for vampire werewolves problem is defined as [NumVampInEast, NumWerewolfInEast, NumVampInWest, NumWerewolfWest, DirBoar].
Direction of boat is represented as 'e' or 'w'.

In order to implement BFS in these two problems use the following command in the prolog terminal:
goBFS(Puzzle-Name, Start-State, Goal-State).

Example:
goBFS(vamp-wolves, [3,3,0,0,e], [0,0,3,3,w]).
goBFS(sliding-tile, [1,0,2,4,5,6,3,7,8,9,10,11,12,13,14,15], [1,2,3,4,5,6,0,7,8,9,10,11,12,13,14,15]).


In order to implement DFS in these two problems use the following command in the prolog terminal:
goBFS(Puzzle-Name, Start-State, Goal-State).

Example:
goDFS(vamp-wolves, [3,3,0,0,e], [0,0,3,3,w]).
goDFS(sliding-tile, [1,2,0,4,5,6,3,7,8,9,10,11,12,13,14,15], [1,2,3,4,5,6,0,7,8,9,10,11,12,13,14,15]).


In order to implement HFS in these two problems use the following command in the prolog terminal:
goHFS(Puzzle-Name, Start-State, Goal-State).

Example:
goHFS(vamp-wolves, [3,3,0,0,e], [0,0,3,3,w]).
goHFS(sliding-tile, [1,6,2,4,5,0,3,7,8,9,10,11,12,13,14,15], [1,2,3,4,5,6,0,7,8,9,10,11,12,13,14,15]).

