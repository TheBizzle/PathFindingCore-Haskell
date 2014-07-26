module PathFindingCore.Coordinate where

  data Coordinate
    = BadCoord
    | Coord { x :: Int, y :: Int }

  isValid :: Coordinate -> Bool
  isValid BadCoord = False
  isValid _        = True

  overlaps :: Coordinate -> Coordinate -> Bool
  overlaps BadCoord _        = False
  overlaps _        BadCoord = False
  overlaps a        b        = a == b

  instance Eq Coordinate where
    BadCoord      == BadCoord      = True
    (Coord x1 y1) == (Coord x2 y2) = x1 == x2 && y1 == y2
    _             == _             = False

  data PriorityCoordinate
    = PCoord { priority :: Int, coord :: Coordinate }

  compare :: PriorityCoordinate -> PriorityCoordinate -> Bool
  compare (PCoord p1 _) (PCoord p2 _) = p1 < p2

  data Breadcrumb
    = Breadcrumb { to :: Coordinate, from :: Coordinate }
