{-# OPTIONS_GHC -Wall #-}

module Data (
  Size(..),
  Pos(..),
  Link(..),
  Puzzle(..),
  mapLinks
  ) where

import Control.Applicative
import Data.List
import Data.Maybe

data Size = Size
  { getWidth, getHeight :: Integer
  } deriving (Show, Read, Eq)

data Pos = Pos 
  { getX, getY :: Integer
  } deriving (Read, Eq)

instance Show Pos where
  show (Pos x y) = "Pos " ++ show x ++ " " ++ show y

data Link a = Link
  { getType      :: a
  , getPositions :: [Pos]
  } deriving (Show, Read)

data Puzzle a = Puzzle
  { getSize  :: Size
  , getLinks :: [Link a]
  } deriving (Read)

instance (Show a) => Show (Puzzle a) where
  show = showPuzzle

showPuzzle :: (Show a) => Puzzle a -> String
showPuzzle = init . unlines . map toChars . toMaybes
     
toChars :: (Show a) => [Maybe a] -> [Char]
toChars = intersperse ' ' . map maybeToChar

maybeToChar :: (Show a) => Maybe a -> Char
maybeToChar = fromMaybe ' ' . (>>= listToMaybe . show)

toMaybes :: Puzzle a -> [[Maybe a]]
toMaybes = map' <$> findValue . getLinks <*> genPos . getSize

findValue :: [Link a] -> Pos -> Maybe a
findValue = flip findValue'

findValue' :: Pos -> [Link a] -> Maybe a
findValue' = fmap (listToMaybe . map getType) . filter . containsPos

containsPos :: Pos -> Link a -> Bool
containsPos p = (p `elem`) . getPositions

genPos :: Size -> [[Pos]]
genPos (Size h b) = map (flip (zipWith Pos) [1..]) rowTable
  where rowTable  = map (replicate . fromInteger $ b) [1..h]

map' :: (a -> b) -> [[a]] -> [[b]]
map' = map . map

mapLinks :: ([Link a] -> [Link b]) -> Puzzle a -> Puzzle b
mapLinks f (Puzzle size links) = Puzzle size (f links)

