{-# OPTIONS_GHC -Wall #-}

{-# LANGUAGE ViewPatterns #-}

module Main where

import System.Environment

import qualified Arukone as A
import Data
import Puzzles

solve :: (Show a) => Puzzle a -> Puzzle a
solve (A.solve -> (x:_)) = x
solve _                  = error "No solutions found!"

main :: IO ()
main = do
  args <- fmap (take 1) getArgs
  case args of
    (arg:_) -> print . solve . premade . read $ arg
    _       -> error "No argument supplied!"

