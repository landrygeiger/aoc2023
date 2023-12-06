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
import Data.Text (Text, pack)
import Control.Applicative
{- ORMOLU_ENABLE -}

runDay :: R.Day
runDay = R.runDay inputParser partA partB

------------ PARSER ------------
parseColor :: Parser Text
parseColor = string (pack "red") <|> string (pack "green") <|> string (pack "blue")

parseCubeCount :: Parser String
parseCubeCount = do
  thiscolor <- parseColor
  _ <- string " "
  count <- decimal
  return thiscolor

inputParser :: Parser Input
inputParser = error "Not implemented yet!"

------------ TYPES ------------

type Input = String

type OutputA = Int

type OutputB = Void

------------ PART A ------------
partA :: Input -> OutputA
partA = error "Not implemented yet!"

------------ PART B ------------
partB :: Input -> OutputB
partB = error "Not implemented yet!"
