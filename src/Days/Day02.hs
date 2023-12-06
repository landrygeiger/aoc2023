module Days.Day02 (runDay) where

{- ORMOLU_DISABLE -}
import Data.List
import Data.Map.Strict (Map)
import qualified Data.Map.Strict as Map
import Data.Maybe
import Data.Set (Set)
import qualified Data.Set as Set
import Data.Vector (Vector)
import qualified Data.Vector as Vec
import qualified Util.Util as U

import qualified Program.RunDay as R (runDay, Day)
import Data.Attoparsec.Text
import Data.Void
import Data.Text (Text, pack, unpack)
import Control.Applicative
{- ORMOLU_ENABLE -}

runDay :: R.Day
runDay = R.runDay inputParser partA partB

------------ PARSER ------------
color :: Parser Text
color = string (pack "red") <|> string (pack "green") <|> string (pack "blue")

cubeCount :: Parser CubeCount
cubeCount = do
  thisNumCubes <- decimal
  string " "
  thisColor <- color
  return CubeCount { cubeColor = thisColor, numCubes = thisNumCubes }

cubeCounts :: Parser [CubeCount]
cubeCounts = cubeCount `sepBy1` string (pack ", ")

combineCubeCount :: CubeCount -> Round -> Round
combineCubeCount cc round = case unpack . cubeColor $ cc of
  "red" -> round { red = numCubes cc }
  "green" -> round { green = numCubes cc }
  "blue" -> round { blue = numCubes cc }

round :: Parser Round
round = do
  foldr combineCubeCount Round { red = 0, green = 0, blue = 0 } <$> cubeCounts

rounds :: Parser [Round]
rounds = Days.Day02.round `sepBy1` string (pack "; ")

game :: Parser Game
game = do
  string . pack $ "Game "
  thisId <- decimal
  string . pack $ ": "
  thisRounds <- rounds
  return $ Game { gameId = thisId, gameRounds = thisRounds }

games :: Parser [Game]
games = game `sepBy1` endOfLine

inputParser :: Parser Input
inputParser = games

------------ TYPES ------------
data CubeCount = CubeCount { cubeColor :: Text, numCubes :: Int }

data Round = Round { red :: Int, green :: Int, blue :: Int } deriving Show

data Game = Game { gameId :: Int, gameRounds :: [Round] } deriving Show

type Input = [Game]

type OutputA = Int

type OutputB = Int

------------ PART A ------------
roundPossible :: Round -> Bool
roundPossible r = red r <= 12 && green r <= 13 && blue r <= 14

gamePossible :: Game -> Bool
gamePossible g = all roundPossible $ gameRounds g

partA :: Input -> OutputA
partA = sum . map gameId . filter gamePossible

------------ PART B ------------
minCubes :: [Round] -> Round
minCubes =
  let
    fold curr acc = Round { red = max (red curr) (red acc), green = max (green curr) (green acc), blue = max (blue curr) (blue acc)}
  in foldr fold Round { red = 0, green = 0, blue = 0 }

cubesPower :: Round -> Int
cubesPower r = red r * green r * blue r

partB :: Input -> OutputB
partB = sum . map (cubesPower . minCubes . gameRounds)
