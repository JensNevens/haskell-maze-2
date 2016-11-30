{-# LANGUAGE MultiParamTypeClasses #-}

module Instance1 (Maze(..)) where

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
        | paths == [] = Nothing
        | otherwise = Just $ minimumBy (comparing length) paths
      where
        paths = allPaths (Board rows cols entrance exits graph) [entrance] []

    longest (Board _ _ _ [] _) = Nothing
    longest (Board rows cols entrance exits graph)
        | paths == [] = Nothing
        | otherwise = Just $ maximumBy (comparing length) paths
      where
        paths = allPaths (Board rows cols entrance exits graph) [entrance] []

  allPaths :: Board -> [Position] -> [Position] -> [[Position]]
  allPaths board path visited = do
      succNode <- neighbours board currNode
      guard (not $ succNode `elem` visited)
      go board path visited currNode succNode
    where
      currNode = head path
      go (Board rows cols entrance exits graph) path visited currNode succNode
        | succNode `elem` exits = return $ reverse $ (succNode:path)
        | otherwise = allPaths (Board rows cols entrance exits graph)
                               (succNode:path)
                               (currNode:visited)

  instance Show Board where
    show (Board rows cols entrance exits graph) =
        concat $ map (++"\n") strings
      where
        path = shortest (Board rows cols entrance exits graph)
        strings = map (\r ->
                      map (\c ->
                          getSymbol (Board rows cols entrance exits graph)
                                    (Position (r,c))
                                    path)
                          [0..cols-1])
                      [0..rows-1]
