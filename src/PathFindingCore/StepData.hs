module PathFindingCore.StepData where

  import Data.Array.IArray

  import PathFindingCore.PathingMap.Coordinate
  import PathFindingCore.PathingMap.Interpreter(PathingGrid)

  data StepData
    = StepData {
      loc           :: Coordinate,
      endCoord      :: Coordinate,
      grid          :: PathingGrid,
      breadcrumbArr :: Array (Int, Int) (Coordinate, Coordinate),
      endGoal       :: Coordinate
    }
