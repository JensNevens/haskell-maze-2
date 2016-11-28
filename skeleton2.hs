{-# LANGUAGE MultiParamTypeClasses #-}

import qualified System.Environment
import qualified Data.Map.Strict as Map

main :: IO ()
main = do [path] <- System.Environment.getArgs
          maze <- readFile path
          --putStr $ show $ shortest $ read maze
          putStr $ show $ (read maze :: Board)

class (Read board, Show position) => Maze board position where
  entrance :: board -> position
  exits :: board -> [position]
  neighbours :: board -> position -> Maybe [position]
  shortest :: board -> Maybe [position]

data Position = Position (Int, Int) deriving (Show)

instance Eq Position where
  (==) (Position (x1,y1)) (Position (x2,y2)) =
    (==) (x1, y1) (x2, y2)

instance Ord Position where
  compare (Position (x1,y1)) (Position (x2,y2)) =
    compare (x1,y1) (x2,y2)

data Board = Board Position [Position] (Map.Map Position [Position]) deriving (Show)

instance Read Board where
  readsPrec _ input =
    let maze = lines input
        entrance = head $ elemIndexes maze "*"
        frees = elemIndexes maze " "
        exits = elemIndexes maze "@"
        locations = [entrance] ++ frees ++ exits
        mapdata = [ (p, freeNeighbours p locations) | p <- locations ]
    in [((Board entrance exits (Map.fromList mapdata)), "")]

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
freeNeighbours current free = filter (\x -> isNeighbour current x) free

isNeighbour :: Position -> Position -> Bool
isNeighbour (Position (r1,c1)) (Position (r2,c2)) =
  (r1 == r2 && abs (c1 - c2) == 1) || (c1 == c2 && abs (r1 - r2) == 1)

instance Maze Board Position where
  entrance (Board entrance _ _) = entrance
  exits (Board _ exits _) = exits
  neighbours (Board _ _ dict) pos = Map.lookup pos dict
