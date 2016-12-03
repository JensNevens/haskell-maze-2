
module Position (Position(..)) where

  newtype Position = Position (Int, Int)
                     deriving (Show, Eq)
