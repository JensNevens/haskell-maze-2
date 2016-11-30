{-# LANGUAGE MultiParamTypeClasses #-}

import qualified System.Environment
import qualified Data.Map.Strict as Map
import Control.Monad
import Data.List (minimumBy, maximumBy)
import Data.Ord (comparing)

main :: IO ()
main = do [path] <- System.Environment.getArgs
          maze <- readFile path
          putStr $ show $ (shortest $ (read maze :: Board) :: Maybe [Position])

class (Read board, Show position) => Maze board position where
  entrance :: board -> position
  exits :: board -> [position]
  neighbours :: board -> position -> [position]
  shortest :: board -> Maybe [position]
  longest :: board -> Maybe [position]

newtype Position = Position (Int, Int) -- (row, column)
                   deriving (Show, Eq, Ord)

data Board = Board Position -- entrance
                   [Position] -- exits
                   (Map.Map Position [Position]) -- adjacancy list
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

  -- shortest (Board entrance [] graph) = Nothing
  -- shortest (Board entrance exits graph)
  --     | paths == [] = Nothing
  --     | otherwise = Just $ minimumBy (comparing length) paths
  --   where
  --     paths = allPaths (Board entrance exits graph) [entrance] []
  shortest (Board entrance [] graph) = Nothing
  shortest (Board entrance exits graph)
      | path == [] = Nothing
      | otherwise = Just path
    where
      path = bfs (Board entrance exits graph) [[entrance]] []

  longest (Board entrance [] graph) = Nothing
  longest (Board entrance exits graph)
      | paths == [] = Nothing
      | otherwise = Just $ maximumBy (comparing length) paths
    where
      paths = allPaths (Board entrance exits graph) [entrance] []

allPaths :: Board -> [Position] -> [Position] -> [[Position]]
allPaths board path visited = do
    succNode <- neighbours board currNode
    guard (not $ succNode `elem` visited)
    go board path visited currNode succNode
  where
    currNode = head path
    go (Board entrance exits graph) path visited currNode succNode
      | succNode `elem` exits = return $ reverse $ (succNode:path)
      | otherwise = allPaths (Board entrance exits graph)
                             (succNode:path)
                             (currNode:visited)

bfs :: Board -> [[Position]] -> [Position] -> [Position]
bfs (Board entrance exits graph) [] visited = []
bfs (Board entrance exits graph) paths visited
    | currNode `elem` exits = reverse path
    | not $ currNode `elem` visited = bfs (Board entrance exits graph)
                                          ((tail paths)++newPaths)
                                          (currNode:visited)
    | otherwise = bfs (Board entrance exits graph)
                      (tail paths)
                      visited
  where
    path = head paths
    currNode = head path
    ns = neighbours (Board entrance exits graph) currNode
    newPaths = [ (n:path) | n <- ns ]
