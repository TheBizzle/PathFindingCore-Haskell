module PathFindingCore.PathingMap.Coordinate where

import Data.Ix(Ix)

data Coordinate
  = Coord { x :: Int, y :: Int } deriving (Eq, Ix, Ord, Show)

data Breadcrumb
  = Crumb { to :: Coordinate, from :: Breadcrumb }
  | Source { source :: Coordinate } deriving (Eq, Show)

breadcrumbsToList :: Breadcrumb -> [Coordinate]
breadcrumbsToList (Source s) = [s]
breadcrumbsToList x          = reverse $ helper x
  where
    helper src@(Source _)  = breadcrumbsToList src
    helper (Crumb to from) = to : (helper from)
