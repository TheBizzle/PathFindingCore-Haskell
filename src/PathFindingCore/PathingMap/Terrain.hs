module PathFindingCore.PathingMap.Terrain where

  data Terrain
    = Ant
    | Empty
    | Food
    | Goal
    | Mound
    | Invalid
    | Path
    | Query
    | Self
    | Wall
    | Water

  isPassable :: Terrain -> Bool
  isPassable Ant     = True
  isPassable Empty   = True
  isPassable Food    = True
  isPassable Goal    = True
  isPassable Mound   = True
  isPassable Invalid = False
  isPassable Path    = False
  isPassable Query   = False
  isPassable Self    = False
  isPassable Wall    = False
  isPassable Water   = False

  charToTerrain :: Char -> Terrain
  charToTerrain 'a' = Ant
  charToTerrain '_' = Empty
  charToTerrain 'f' = Food
  charToTerrain 'G' = Goal
  charToTerrain 'O' = Mound
  charToTerrain 'x' = Path
  charToTerrain '.' = Query
  charToTerrain '*' = Self
  charToTerrain 'D' = Wall
  charToTerrain '%' = Water
  charToTerrain _   = Invalid

  terrainToChar :: Terrain -> Char
  terrainToChar Ant     = 'a'
  terrainToChar Empty   = '_'
  terrainToChar Food    = 'f'
  terrainToChar Goal    = 'G'
  terrainToChar Mound   = 'O'
  terrainToChar Path    = 'x'
  terrainToChar Query   = '.'
  terrainToChar Self    = '*'
  terrainToChar Wall    = 'D'
  terrainToChar Water   = '%'
  terrainToChar Invalid = error "`Invalid` is not representable terrain"
