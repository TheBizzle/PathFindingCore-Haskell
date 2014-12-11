module PathFindingCore.Status where

data Status
  = Failure
  | Success
  | Continue

data RunResult
  = FailedRun
  | SuccessfulRun
