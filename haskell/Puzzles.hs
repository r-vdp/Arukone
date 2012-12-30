{-# OPTIONS_GHC -Wall #-}

module Puzzles (premade) where

import Data

premade :: Integer -> Puzzle Integer
premade ix = case ix of
  1  -> Puzzle (Size 5 5) [ Link 1 [Pos 2 1, Pos 5 5]
                          , Link 2 [Pos 1 5, Pos 3 2]
                          , Link 3 [Pos 1 3, Pos 3 3]
                          , Link 4 [Pos 1 1, Pos 2 3]
                          ]
  3  -> Puzzle (Size 6 6) [ Link 1 [Pos 1 3, Pos 3 1]
                          , Link 2 [Pos 2 2, Pos 5 5]
                          , Link 3 [Pos 2 3, Pos 3 6]
                          , Link 4 [Pos 2 5, Pos 5 1]
                          , Link 5 [Pos 5 2, Pos 6 1]
                          ]
  6  -> Puzzle (Size 7 7) [ Link 1 [Pos 2 1, Pos 7 1]
                          , Link 2 [Pos 3 3, Pos 4 5]
                          , Link 3 [Pos 5 3, Pos 6 5]
                          , Link 4 [Pos 5 5, Pos 6 2]
                          , Link 5 [Pos 6 1, Pos 6 6]
                          ]
  9  -> Puzzle (Size 8 8) [ Link 1 [Pos 1 5, Pos 1 7]
                          , Link 2 [Pos 1 6, Pos 4 6]
                          , Link 3 [Pos 2 5, Pos 7 4]
                          , Link 4 [Pos 2 7, Pos 4 7]
                          , Link 5 [Pos 3 3, Pos 5 3]
                          , Link 6 [Pos 5 4, Pos 7 2]
                          , Link 7 [Pos 6 6, Pos 7 3]
                          ]
  10 -> Puzzle (Size 8 8) [ Link 1 [Pos 1 1, Pos 7 8]
                          , Link 2 [Pos 2 7, Pos 4 7]
                          , Link 3 [Pos 2 8, Pos 4 2]
                          , Link 4 [Pos 3 4, Pos 4 8]
                          , Link 5 [Pos 5 5, Pos 7 3]
                          , Link 6 [Pos 5 8, Pos 6 3]
                          , Link 7 [Pos 6 8, Pos 7 4]
                          ]
  11 -> Puzzle (Size 9 9) [ Link 1 [Pos 8 6, Pos 5 8]
                          , Link 2 [Pos 2 8, Pos 9 9]
                          , Link 3 [Pos 8 2, Pos 3 5]
                          , Link 4 [Pos 1 5, Pos 8 7]
                          , Link 5 [Pos 6 2, Pos 2 3]
                          , Link 6 [Pos 1 8, Pos 3 9]
                          , Link 7 [Pos 3 1, Pos 2 2]
                          , Link 8 [Pos 4 3, Pos 7 4]
                          , Link 9 [Pos 2 1, Pos 9 4]
                          ]
  _  -> error "Undefined puzzle!"

