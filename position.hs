
module Position (Position(..)) where

  newtype Position = Position (Int, Int) -- (row, column)
                     deriving (Show, Eq, Ord)
