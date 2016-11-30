
module Board (Board(..), getSymbol) where

  import qualified Data.Map.Strict as Map

  import Position

  data Board = Board Int -- rows
                     Int -- columns
                     Position -- entrance
                     [Position] -- exits
                     (Map.Map Position [Position]) -- adjacancy list

  instance Read Board where
    readsPrec _ input =
      let maze = lines input
          rows = length maze
          cols = length $ head maze
          entrance = head $ elemIndexes maze "*"
          frees = elemIndexes maze " "
          exits = elemIndexes maze "@"
          nodes = [entrance] ++ frees ++ exits
          graph = [ (n, freeNeighbours n nodes) | n <- nodes ]
      in [((Board rows cols entrance exits (Map.fromList graph)), "")]

  getSymbol :: Board -> Position -> Maybe [Position] -> Char
  getSymbol (Board _ _ entrance exits graph) pos Nothing
    | pos == entrance = '*'
    | pos `elem` exits = '@'
    | pos `elem` (Map.keys graph) = ' '
    | otherwise = 'X'
  getSymbol (Board _ _ entrance exits graph) pos (Just path)
    | pos == entrance = '*'
    | pos `elem` exits = '@'
    | pos `elem` path = '.'
    | pos `elem` (Map.keys graph) = ' '
    | otherwise = 'X'

  elemIndexes :: [[Char]] -> [Char] -> [Position]
  elemIndexes maze item =
    let rows = length maze
        cols = length $ head maze
    in [ Position (r,c) | r <- [0..rows-1],
                          c <- [0..cols-1],
                          getItem maze (r,c) == item]

  getItem :: [[Char]] -> (Int,Int) -> [Char]
  getItem maze (r,c) = [maze !! r !! c]

  freeNeighbours :: Position -> [Position] -> [Position]
  freeNeighbours current free =
    filter (\x -> isNeighbour current x) free

  isNeighbour :: Position -> Position -> Bool
  isNeighbour (Position (r1,c1)) (Position (r2,c2)) =
    (r1 == r2 && abs (c1 - c2) == 1)
    || (c1 == c2 && abs (r1 - r2) == 1)
