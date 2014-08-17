module PathFindingCore.PathingMap.Coordinate where

  import Data.Ix

  data Coordinate
    = Coord { x :: Int, y :: Int } deriving (Eq, Ix, Ord, Show)

  data PriorityCoordinate
    = PCoord { priority :: Int, coord :: Coordinate } deriving (Eq, Show)

  instance Ord PriorityCoordinate where
    compare (PCoord p1 _) (PCoord p2 _) = compare p1 p2

  data Breadcrumb
    = Breadcrumb { to :: Coordinate, from :: Coordinate }
