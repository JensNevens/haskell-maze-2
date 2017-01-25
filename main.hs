
import qualified System.Environment
import Position
import Maze
import MMaze
import CMaze

main :: IO ()
main = do [path] <- System.Environment.getArgs
          maze <- readFile path
          putStrLn "Monad Board Solution:"
          putStr $ show $ (shortest $ (read maze :: MBoard) :: Maybe [Position])
          putStrLn "Char Board Solution:"
          putStr $ show $ (shortest $ (read maze :: CBoard) :: Maybe [Position])
