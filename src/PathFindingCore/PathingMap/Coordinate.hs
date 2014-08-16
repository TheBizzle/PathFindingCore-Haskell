module PathFindingCore.PathingMap.Coordinate where

  data Coordinate
    = Coord { x :: Int, y :: Int } deriving (Eq, Show)

  data PriorityCoordinate
    = PCoord { priority :: Int, coord :: Coordinate }

  compare :: PriorityCoordinate -> PriorityCoordinate -> Bool
  compare (PCoord p1 _) (PCoord p2 _) = p1 < p2

  data Breadcrumb
    = Breadcrumb { to :: Coordinate, from :: Coordinate }
