{-# LANGUAGE MultiParamTypeClasses #-}

module Instance2 (Maze(..)) where

  import qualified Data.Map.Strict as Map
  import Data.List (find)

  import Board
  import Position
  import Maze

  instance Maze Board Position where
    entrance (Board _ _ entrance _ _) = entrance
    exits (Board _ _ _ exits _) = exits

    neighbours (Board _ _ _ _ graph) pos =
        go $ Map.lookup pos graph
      where
        go (Just ns) = ns
        go Nothing = []

    shortest (Board _ _ _ [] _) = Nothing
    shortest (Board rows cols entrance exits graph)
        | path == [] = Nothing
        | otherwise = Just path
      where
        path = bfs (Board rows cols entrance exits graph)

  bfs :: Board -> [Position]
  bfs (Board rows cols entrance exits graph) =
    searchBFS (Board rows cols entrance exits graph) [[entrance]] exits []

  searchBFS :: Board -> [[Position]] -> [Position] -> [Position] -> [Position]
  searchBFS _ [] _ _ = []
  searchBFS board frontier exits visited =
      go $ find ((`elem` exits) . head) frontier
    where
      go (Just exitPath) = reverse exitPath
      go Nothing = searchBFS board sucFrontier exits (visited++curFrontier)
      curFrontier = map head frontier
      sucFrontier = [ (suc:cur) | cur <- frontier,
                                  suc <- neighbours board (head cur),
                                  not (suc `elem` visited)]
