module PathFindingCore.PathingMap.Coordinate where

  import Data.Ix

  data Coordinate
    = Coord { x :: Int, y :: Int } deriving (Eq, Ix, Ord, Show)

  data Breadcrumb
    = Crumb { to :: Coordinate, from :: Breadcrumb }
    | Source { source :: Coordinate } deriving (Eq, Show)
