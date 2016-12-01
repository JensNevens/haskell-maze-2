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
        go $ Map.lookup pos graph
      where
        go (Just ns) = ns
        go Nothing = []

    shortest (Board _ _ _ [] _) = Nothing
    shortest (Board rows cols entrance exits graph)
        | sols == [] = Nothing
        | otherwise = Just $ minimumBy (comparing length) sols
      where
        sols = solve (Board rows cols entrance exits graph) [entrance] exits []

    longest (Board _ _ _ [] _) = Nothing
    longest (Board rows cols entrance exits graph)
        | sols == [] = Nothing
        | otherwise = Just $ maximumBy (comparing length) sols
      where
        sols = solve (Board rows cols entrance exits graph) [entrance] exits []

  solve :: Board -> [Position] -> [Position] -> [Position] -> [[Position]]
  solve board path exits visited = do
      suc <- neighbours board cur
      guard (not $ suc `elem` visited)
      go suc
    where
      cur = head path
      go suc
        | suc `elem` exits = return $ reverse $ (suc:path)
        | otherwise = solve board (suc:path) exits (cur:visited)

  -- instance Show Board where
  --   show (Board rows cols entrance exits graph) =
  --       concat $ map (++"\n") strings
  --     where
  --       path = shortest (Board rows cols entrance exits graph)
  --       strings = map (\r ->
  --                     map (\c ->
  --                         getSymbol (Board rows cols entrance exits graph)
  --                                   (Position (r,c))
  --                                   path)
  --                         [0..cols-1])
  --                     [0..rows-1]
