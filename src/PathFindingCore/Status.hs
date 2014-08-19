module PathFindingCore.Status where

  data Status t
    = Failure  { result :: t }
    | Success  { result :: t }
    | Continue { result :: t }
