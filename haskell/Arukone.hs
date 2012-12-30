{-# OPTIONS_GHC -Wall #-}

module Arukone (solve) where

import Control.Applicative
import Control.Monad
import Data.Function
import Data.List
import Data.Monoid

import Data
import Occupied

solve :: Puzzle a -> [Puzzle a]
solve = findPaths . optimise

findPaths :: Puzzle a -> [Puzzle a]
findPaths (Puzzle size links) = toPuzzle paths
  where toPuzzle = map (Puzzle size)
        paths    = findPaths' size links (initOccupied links)

findPaths' :: Size -> [Link a] -> Occupied -> [[Link a]]
findPaths' _    []    _   = [[]]
findPaths' size (l:ls) occ = let Link a [from,to] = l in do
    (path, occ') <- findPath from to size occ
    guard . not . null $ path
    paths <- findPaths' size ls occ'
    return $ Link a path : paths

initOccupied :: [Link a] -> Occupied
initOccupied = foldr (flip add') emptyOcc . map getPositions

findPath :: Pos -> Pos -> Size -> Occupied -> [([Pos], Occupied)]
findPath from to size occ = findPath' from to size occ []

findPath' :: Pos -> Pos -> Size -> Occupied -> [Pos] -> [([Pos], Occupied)]
findPath' from to size occ acc
  | from <~> to = return (to : from : acc, occ)
  | otherwise   = do
      n <- sortedNeighbours size from
      guard . not . (isOcc occ) $ n
      findPath' n to size (add occ n) (from : acc)

sortedNeighbours :: Size -> Pos -> [Pos]
sortedNeighbours size = sortNeighbours size . neighbours size

neighbours :: Size -> Pos -> [Pos]
neighbours size pos = filter (inBounds size) (candidates pos)
  where inBounds (Size w h) (Pos x y) = x > 0 && x <= w && y > 0 && y <= h
        candidates (Pos x y) = [Pos (x-1) y, Pos (x+1) y, Pos x (y-1), Pos x (y+1)]

-- next to
(<~>) :: Pos -> Pos -> Bool
(Pos x1 y1) <~> (Pos x2 y2) = check x1 x2 y1 y2 || check y1 y2 x1 x2
  where check x x' y y' = abs (x-x') == 1 && y == y'


------------------ Sorting --------------------------------------

optimise :: Puzzle a -> Puzzle a
optimise p = mapLinks (sortLinks . getSize $ p) p

sortLinks :: Size -> [Link a] -> [Link a]
sortLinks size = sortBy comparison
  where comparison = mappend <$> onBorderComparison size <*> distanceComparison

onBorderComparison :: Size -> Link a -> Link a -> Ordering
onBorderComparison size = compare `on` negate . (linkBorderScore size)

linkBorderScore :: Size -> Link a -> Integer
linkBorderScore size = sum . map (borderScore size) . getPositions

onBorder :: Size -> Pos -> Bool
onBorder (Size w h) (Pos x y) = x == 1 || y == 1 || x == w || y == h

distanceComparison :: Link a -> Link a -> Ordering
distanceComparison = compare `on` (negate . distance)

distance :: Link a -> Integer
distance (Link _ (from:to:_)) = let Pos x1 y1 = from
                                    Pos x2 y2 = to
                                in  abs (x1 - x2) + abs (y1 - y2)
distance _                    = error "Something went terribly wrong!"


sortNeighbours :: Size -> [Pos] -> [Pos]
sortNeighbours size = sortBy (compare `on` (negate . borderScore size))

borderScore :: Size -> Pos -> Integer
borderScore size pos 
  | onBorder size pos = 1
  | otherwise         = 0

