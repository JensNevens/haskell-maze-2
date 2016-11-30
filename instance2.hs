{-# LANGUAGE MultiParamTypeClasses #-}

module Instance2 (Maze(..)) where

  import Control.Monad
  import Data.List (minimumBy, maximumBy)
  import Data.Ord (comparing)
  import qualified Data.Map.Strict as Map

  import Board
  import Position
  import Maze

  instance Maze Board Position where
    entrance (Board _ _ entrance _ _) = entrance
    exits (Board _ _ _ exits _) = exits

    neighbours (Board _ _ _ _ graph) pos =
        getNeighbours $ Map.lookup pos graph
      where
        getNeighbours (Just ns) = ns
        getNeighbours Nothing = []

    shortest (Board _ _ _ [] _) = Nothing
    shortest (Board rows cols entrance exits graph)
        | path == [] = Nothing
        | otherwise = Just path
      where
        path = bfs (Board rows cols entrance exits graph) [[entrance]] []

  bfs :: Board -> [[Position]] -> [Position] -> [Position]
  bfs (Board _ _ _ _ _) [] visited = []
  bfs (Board rows cols entrance exits graph) paths visited
      | currNode `elem` exits = reverse path
      | currNode `elem` visited = bfs (Board rows cols entrance exits graph)
                                      (tail paths)
                                      visited
      | otherwise = bfs (Board rows cols entrance exits graph)
                        ((tail paths)++newPaths)
                        (currNode:visited)
    where
      path = head paths
      currNode = head path
      ns = neighbours (Board rows cols entrance exits graph) currNode
      newPaths = [ (n:path) | n <- ns ]
