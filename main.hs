
import qualified System.Environment

import Position
import Board
import Instance1
-- import Instance2

main :: IO ()
main = do [path] <- System.Environment.getArgs
          maze <- readFile path
          --putStr $ show $ (read maze :: Board)
          putStr $ show $ (shortest $ (read maze :: Board) :: Maybe [Position])
