{-# LANGUAGE MultiParamTypeClasses #-}

module CMaze (CBoard(..), Maze(..)) where

  import qualified System.Environment
  import Data.List (find)
  import Maze
  import Position

  newtype CBoard = CBoard [[Char]]
                   deriving (Show)

  instance Read CBoard where
    readsPrec _ input =
      let maze = lines input
      in [(CBoard maze, "")]

  instance Maze CBoard Position where
    entrance board = head $ index board '*'

    exits board = index board '@'

    blanks board = index board ' '

    neighbours board pos =
        getNeighbours pos nodes
      where
        nodes = [entrance board] ++ (exits board) ++ (blanks board)

    shortest board
        | path == [] = Nothing
        | otherwise = Just path
      where
        path = searchBFS board [[entrance board]] (exits board) []

    longest board = Nothing

  index :: CBoard -> Char -> [Position]
  index (CBoard maze) item =
    let rows = length maze
        cols = length $ head maze
    in [ Position (r,c) | r <- [0..rows-1],
                          c <- [0..cols-1],
                          (CBoard maze) !!! Position (r,c) == item ]

  (!!!) :: CBoard -> Position -> Char
  (!!!) (CBoard maze) (Position (r,c)) = maze !! r !! c

  getNeighbours :: Position -> [Position] -> [Position]
  getNeighbours current free =
    filter (nextto current) free

  nextto :: Position -> Position -> Bool
  nextto (Position (r1,c1)) (Position (r2,c2)) =
    (r1 == r2 && abs (c1 - c2) == 1)
    || (c1 == c2 && abs (r1 - r2) == 1)

  getSymbol :: CBoard -> Position -> Maybe [Position] -> Char
  getSymbol board pos Nothing =
    board !!! pos
  getSymbol board pos (Just path)
    | pos == (entrance board) = '*'
    | pos `elem` (exits board) = '@'
    | pos `elem` path = '.'
    | otherwise = board !!! pos

  searchBFS :: CBoard -> [[Position]] -> [Position] -> [Position] -> [Position]
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

  printPath :: CBoard -> Maybe [Position] -> String
  printPath (CBoard maze) path =
        concat $ map (++"\n") strings
      where
        rows = length maze
        cols = length $ head maze
        strings = map (\r ->
                      map (\c ->
                            getSymbol (CBoard maze) (Position (r,c)) path)
                          [0..cols-1])
                      [0..rows-1]
