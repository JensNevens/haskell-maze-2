{-# LANGUAGE MultiParamTypeClasses #-}

module Maze (Maze(..)) where

  class (Read board, Show position) => Maze board position where
    entrance :: board -> position
    exits :: board -> [position]
    neighbours :: board -> position -> [position]
    shortest :: board -> Maybe [position]
    longest :: board -> Maybe [position]
