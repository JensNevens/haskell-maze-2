{-# LANGUAGE MultiParamTypeClasses #-}

module MMaze (MBoard(..), Maze(..)) where

  import Control.Monad
  import Data.List (minimumBy, maximumBy)
  import Data.Ord (comparing)
  import Maze
  import Position

  data MazeSymbol = Entrance | Exit | Blank
                    deriving (Show, Eq)

  data BoardEntry = Entry {
                      location :: Position,
                      symbol :: MazeSymbol,
                      neighbors :: [Position]}
                    deriving (Show)

  newtype MBoard = Board [BoardEntry]
                  deriving (Show)

  instance Read MBoard where
    readsPrec _ input =
      let maze = lines input
          entrance = index maze Entrance
          blanks = index maze Blank
          exits = index maze Exit
          nodes = entrance ++ blanks ++ exits
          entries = [ Entry n
                            (fromChar $ maze !!! n)
                            (getNeighbours n nodes)
                      | n <- nodes ]
      in [(Board entries, "")]

  index :: [[Char]] -> MazeSymbol -> [Position]
  index maze item =
    let rows = length maze
        cols = length $ head maze
    in [ Position (r,c) | r <- [0..rows-1],
                          c <- [0..cols-1],
                          maze !!! Position (r,c) == toChar item ]

  (!!!) :: [[Char]] -> Position -> Char
  (!!!) maze (Position (r,c)) = maze !! r !! c

  fromChar :: Char -> MazeSymbol
  fromChar '*' = Entrance
  fromChar '@' = Exit
  fromChar ' ' = Blank

  toChar :: MazeSymbol -> Char
  toChar Entrance = '*'
  toChar Exit = '@'
  toChar Blank = ' '

  getNeighbours :: Position -> [Position] -> [Position]
  getNeighbours current free =
    filter (nextto current) free

  nextto :: Position -> Position -> Bool
  nextto (Position (r1,c1)) (Position (r2,c2)) =
    (r1 == r2 && abs (c1 - c2) == 1)
    || (c1 == c2 && abs (r1 - r2) == 1)

  instance Maze MBoard Position where
    entrance (Board entries) =
      location $ head $ filter ((== Entrance) . symbol) entries

    exits (Board entries) =
      map location $ filter ((== Exit) . symbol) entries

    blanks (Board entries) =
      map location $ filter ((== Blank) . symbol) entries

    neighbours (Board entries) pos =
      neighbors $ head $ filter ((== pos) . location) entries

    shortest board
        | sols == [] = Nothing
        | otherwise = Just $ minimumBy (comparing length) sols
      where
        sols = solve board [entrance board] (exits board) []

    longest board
        | sols == [] = Nothing
        | otherwise = Just $ maximumBy (comparing length) sols
      where
        sols = solve board [entrance board] (exits board) []

  solve :: MBoard -> [Position] -> [Position] -> [Position] -> [[Position]]
  solve board path exits visited = do
      suc <- neighbours board cur
      guard (not $ suc `elem` visited)
      go suc
    where
      cur = head path
      go suc
        | suc `elem` exits = return $ reverse $ (suc:path)
        | otherwise = solve board (suc:path) exits (cur:visited)
