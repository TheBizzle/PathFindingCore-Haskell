module PathFindingCore.PathingMap.Terrain where

data Terrain
  = Ant
  | Empty
  | Food
  | Goal
  | Mound
  | Path
  | Query
  | Self
  | Wall
  | Water deriving (Eq)

isPassable :: Terrain -> Bool
isPassable Ant   = True
isPassable Empty = True
isPassable Food  = True
isPassable Goal  = True
isPassable Mound = True
isPassable Path  = False
isPassable Query = False
isPassable Self  = False
isPassable Wall  = False
isPassable Water = False

charToTerrain :: Char -> Terrain
charToTerrain 'a' = Ant
charToTerrain ' ' = Empty
charToTerrain 'f' = Food
charToTerrain 'G' = Goal
charToTerrain 'O' = Mound
charToTerrain 'x' = Path
charToTerrain '.' = Query
charToTerrain '*' = Self
charToTerrain 'D' = Wall
charToTerrain '%' = Water
charToTerrain x   = error $ "Cannot convert '" <> (showText x) <> "' to a terrain"

terrainToChar :: Terrain -> Char
terrainToChar Ant   = 'a'
terrainToChar Empty = ' '
terrainToChar Food  = 'f'
terrainToChar Goal  = 'G'
terrainToChar Mound = 'O'
terrainToChar Path  = 'x'
terrainToChar Query = '.'
terrainToChar Self  = '*'
terrainToChar Wall  = 'D'
terrainToChar Water = '%'

instance Show Terrain where
  show = terrainToChar >>> pure
