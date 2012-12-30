{-# OPTIONS_GHC -Wall #-}

module Occupied (
  Occupied,
  emptyOcc,
  add,
  add',
  isOcc
  ) where

import Data

import qualified Data.Map as M

newtype Occupied = Occ {getOccupied :: M.Map Integer [Integer] }
  deriving (Show)

emptyOcc :: Occupied
emptyOcc = Occ M.empty

add :: Occupied -> Pos -> Occupied
add = flip flipped
  where flipped (Pos x y) = Occ . M.insertWith (++) x [y] . getOccupied

add' :: Occupied -> [Pos] -> Occupied
add' = foldr (flip add)

isOcc :: Occupied -> Pos -> Bool
isOcc = flip isOcc'

isOcc' :: Pos -> Occupied -> Bool
isOcc' (Pos x y) = maybe False (y `elem`) . (M.lookup x) . getOccupied

