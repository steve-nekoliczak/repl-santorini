-- HACK: This is here to silence linter warnings.
{-# LANGUAGE OverloadedRecordDot #-}
{-# LANGUAGE StrictData #-}

module Types
  ( XCoord (..)
  , YCoord (..)
  , Position (..)
  , Level (..)
  , Space (..)
  , Player (..)
  , Worker (..)
  , Board (..)
  , BoardError (..)
  , BaseStateT
  , GameState (..)
  , GameStateT
  ) where

import Control.Monad.Trans.State (StateT)
import Data.Map (Map)
import Text.Read (readMaybe)

data XCoord = XA | XB | XC | XD | XE deriving (Show, Eq, Ord, Enum, Bounded)
instance Read XCoord where
  readsPrec _ = convertXCoord
convertXCoord :: String -> [(XCoord, String)]
convertXCoord "A" = [(XA, "")]
convertXCoord "B" = [(XB, "")]
convertXCoord "C" = [(XC, "")]
convertXCoord "D" = [(XD, "")]
convertXCoord "E" = [(XE, "")]
convertXCoord _ = []

data YCoord = Y1 | Y2 | Y3 | Y4 | Y5 deriving (Show, Eq, Ord, Enum, Bounded)
instance Read YCoord where
  readsPrec _ = convertYCoord
convertYCoord :: String -> [(YCoord, String)]
convertYCoord "1" = [(Y1, "")]
convertYCoord "2" = [(Y2, "")]
convertYCoord "3" = [(Y3, "")]
convertYCoord "4" = [(Y4, "")]
convertYCoord "5" = [(Y5, "")]
convertYCoord _ = []

data Position = NotOnBoard | Position (XCoord, YCoord) deriving (Show, Eq, Ord)
instance Read Position where
  readsPrec _ = convertPosition
convertPosition :: String -> [(Position, String)]
convertPosition [xChar, yChar] =
  case (readMaybe[xChar], readMaybe[yChar]) of
    (Just x, Just y)      -> [(Position (x, y), "")]
    (_, _)                -> []
convertPosition _ = []

data Level = Ground | LevelOne | LevelTwo | LevelThree | Dome deriving (Show, Eq, Ord, Enum, Bounded)

data Player = BluePlayer | IvoryPlayer deriving (Show, Eq)
data Worker = BlueMan | BlueWoman | IvoryMan | IvoryWoman deriving (Show, Read, Eq, Ord)

data Space = Space { level :: Level
                   , worker :: Maybe Worker
                   } deriving (Show, Eq)

data Board = Board { grid :: Map Position Space
                   , workers :: Map Worker Position
                   } deriving (Show, Eq)

data BoardError = BuildError String
                | MoveError String
                | OccupiedError String
                | AlreadyPlacedWorkerError String
                | WorkerNotYetPlacedError String
                | TargetSpaceNotAdjacentError String
                | AllWorkersPlacedError String
                | InvalidPositionError String
                | InvalidWorkerError String
                deriving (Show, Eq)

data GameState =
    PlaceWorkers
  | MoveWorker  { player :: Player }
  | BuildUp     { player :: Player, buildWorker :: Worker }
  | GameOver
  deriving (Show, Eq)

type BaseStateT = StateT GameState IO
type GameStateT = BaseStateT (Either BoardError Board)
