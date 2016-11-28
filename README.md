# Haskell Maze 2

_Programming assignment for the course [Functional Programming](https://www.vub.ac.be/en/study/fiches/54625/functional-programming) taught at [VUB SOFT Department](http://soft.vub.ac.be/soft)_

For this second assignment you will have to improve your implementation of the first assignment using Haskell’s module system and type classes. The maze encoding is the same as for the first assignment however you can no longer assume that it is a _perfect_ maze; meaning that walls can be disconnected. There is still exactly one entrance and multiple exits but you can no longer assume that the entrance is at position `(1,1)` and that exits are next to a wall. Finally, finding an escape isn’t good enough; you should find the shortest escape. The length of an escape is to be understood as the number of cells that it composes.

Your project should be entirely written in Haskell and compiled using the Glasgow Haskell Compiler. For this assignment, you should use the Haskell module system in order to split up your solution in multiple files. To complete the assignment you should provide two instances of the `Maze` typeclass. The shortest method may return only the positions where a change of direction occurred. You are free to augment the `Maze` typeclass with additional methods, default implementations and functional dependencies. N.B.: You will also have to insert type annotations inside the main function in order to resolve type ambiguities.

In additions to the requirements detailed above, taking into account the following points will considered as bonuses:

  1. Find two very different data representations for `board` and `position`, be creative!
  2. Modify (slighlty) the `Maze` typeclass such that can display a fancy string representation of the shortest escape.
  3. Implement an efficient algorithm to discover the shortest escape.
  4. Add, implement and test the method `longest` to `Maze` which compute the longest escape (each cell can be visited only once).
