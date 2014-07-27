module PathFindingCore.PathingMap.Coordinate where

  data Coordinate
    = BadCoord
    | Coord { x :: Int, y :: Int } deriving (Eq, Show)

  isValid :: Coordinate -> Bool
  isValid BadCoord = False
  isValid _        = True

  overlaps :: Coordinate -> Coordinate -> Bool
  overlaps BadCoord _        = False
  overlaps _        BadCoord = False
  overlaps a        b        = a == b

  data PriorityCoordinate
    = PCoord { priority :: Int, coord :: Coordinate }

  compare :: PriorityCoordinate -> PriorityCoordinate -> Bool
  compare (PCoord p1 _) (PCoord p2 _) = p1 < p2

  data Breadcrumb
    = Breadcrumb { to :: Coordinate, from :: Coordinate }
