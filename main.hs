
import qualified System.Environment
import Position
import Maze
import MMaze
import CMaze

main :: IO ()
main = do [path] <- System.Environment.getArgs
          maze <- readFile path
          putStr $ show $ (shortest $ (read maze :: MBoard) :: Maybe [Position])
          putStr $ show $ (shortest $ (read maze :: CBoard) :: Maybe [Position])
