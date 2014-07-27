module PathFindingCore.PathingStatus where

  import PathFindingCore.StepData

  data PathingStatus
    = Continue { sd :: StepData }
    | Success  { sd :: StepData }
    | Failure  { sd :: StepData }
