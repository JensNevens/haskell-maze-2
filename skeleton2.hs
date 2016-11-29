{-# LANGUAGE MultiParamTypeClasses #-}

import qualified System.Environment
import qualified Data.Map.Strict as Map
import Control.Monad

main :: IO ()
main = do [path] <- System.Environment.getArgs
          maze <- readFile path
          --putStr $ show $ shortest $ read maze
          putStr $ show $ (read maze :: Board)

class (Read board, Show position) => Maze board position where
  entrance :: board -> position
  exits :: board -> [position]
  neighbours :: board -> position -> [position]
  shortest :: board -> Maybe [position]

newtype Position = Position (Int, Int) -- (row, column)
                   deriving (Show, Eq, Ord)

data Board = Board Position -- entrance
                   [Position] -- exits
                   (Map.Map Position [Position]) -- graph, adjacancy representation
             deriving (Show)

instance Read Board where
  readsPrec _ input =
    let maze = lines input
        entrance = head $ elemIndexes maze "*"
        frees = elemIndexes maze " "
        exits = elemIndexes maze "@"
        nodes = [entrance] ++ frees ++ exits
        graph = [ (n, freeNeighbours n nodes) | n <- nodes ]
    in [((Board entrance exits (Map.fromList graph)), "")]

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
  (r1 == r2 && abs (c1 - c2) == 1) || (c1 == c2 && abs (r1 - r2) == 1)

instance Maze Board Position where
  entrance (Board entrance _ _) = entrance
  exits (Board _ exits _) = exits
  neighbours (Board _ _ graph) pos =
      getNeighbours $ Map.lookup pos graph
    where
      getNeighbours (Just ns) = ns
      getNeighbours Nothing = []

allPaths :: Board -> [Position] -> [Position] -> [[Position]]
allPaths (Board entrance exits graph) path visited = do
    succNode <- neighbours (Board entrance exits graph) currNode
    guard (not $ succNode `elem` visited)
    go (Board entrance exits graph) path visited currNode succNode
  where
    currNode = head path
    go (Board entrance exits graph) path visited currNode succNode
      | succNode `elem` exits = return $ reverse $ (succNode:path)
      | otherwise = allPaths (Board entrance exits graph)
                             (succNode:path)
                             (currNode:visited)
