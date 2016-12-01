
module Board (Board(..), getSymbol) where

  import qualified Data.Map.Strict as Map

  import Position

  data Board = Board Int -- rows/height
                     Int -- columns/width
                     Position -- entrance
                     [Position] -- exits
                     (Map.Map Position [Position]) -- adjacancy list

  instance Read Board where
    readsPrec _ input =
      let maze = lines input
          h = length maze
          w = length $ head maze
          entrance = head $ index maze "*"
          blanks = index maze " "
          exits = index maze "@"
          nodes = [entrance] ++ blanks ++ exits
          graph = [ (n, neighbours n nodes) | n <- nodes ]
      in [((Board h w entrance exits (Map.fromList graph)), "")]

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

  index :: [[Char]] -> [Char] -> [Position]
  index maze item =
    let rows = length maze
        cols = length $ head maze
    in [ Position (r,c) | r <- [0..rows-1],
                          c <- [0..cols-1],
                          maze !!! (r,c) == item]

  (!!!) :: [[Char]] -> (Int,Int) -> [Char]
  (!!!) maze (r,c) = [maze !! r !! c]

  neighbours :: Position -> [Position] -> [Position]
  neighbours current free =
    filter (nextto current) free

  nextto :: Position -> Position -> Bool
  nextto (Position (r1,c1)) (Position (r2,c2)) =
    (r1 == r2 && abs (c1 - c2) == 1)
    || (c1 == c2 && abs (r1 - r2) == 1)
