module Gameplay
  (
  ) where

import Control.Monad.IO.Class (liftIO)
import Control.Monad.Trans.State (StateT, get, put, runStateT)
import Prelude

import GameEngine

data GameState =
    PlaceWorkers
  | MoveWorker  { player :: Player }
  | BuildUp     { player :: Player, worker :: Worker }
  | GameOver
  deriving (Show, Eq)

type BaseStateT = StateT GameState IO
type GameStateT = BaseStateT (Either BoardError Board)

main :: IO ()
main = do
  putStrLn "Starting new game of Santorini!"
  let startGame = gameplayLoopT emptyBoard
  let initialState = PlaceWorkers
  newGame <- runStateT startGame initialState
  print $ fst newGame
  return ()

gameplayLoopT :: Board -> GameStateT
gameplayLoopT board = do
  liftIO $ print board

  state' <- get

  boardAfterAction <-
        case state' of
          PlaceWorkers          -> placeNextWorkerT board
          MoveWorker player     -> moveWorkerT player board
          BuildUp player worker -> buildUpT player worker board
          GameOver              -> return $ Right board

  stateAfterAction <- get

  case boardAfterAction of
    Left errorMessage   -> liftIO (print errorMessage) >> gameplayLoopT board
    Right newBoard      ->
      case stateAfterAction of
        GameOver        -> return boardAfterAction
        _               -> gameplayLoopT newBoard

placeNextWorkerT :: Board -> GameStateT
placeNextWorkerT board = do
  targetPosition <- case nextWorkerToPlace board of
    Just worker -> do
      readPosition $ "Please place " ++ show worker ++ " character"
    Nothing     -> undefined -- TODO: Add exception handling here

  let boardAfterAction = placeNextWorker targetPosition board

  case boardAfterAction of
    Left _            -> put PlaceWorkers
    Right newBoard    ->
      case nextWorkerToPlace newBoard of
        Nothing       -> put $ MoveWorker BluePlayer
        Just _        -> put PlaceWorkers

  return boardAfterAction

moveWorkerT :: Player -> Board -> GameStateT
moveWorkerT player board = do
  let workers = workersForPlayer player

  worker <- readWorker $ "Select a character for " ++ show player ++ ": " ++ show workers
  targetPosition <- readPosition $ "Select target position for " ++ show worker

  let boardAfterAction = moveWorker worker targetPosition board

  case boardAfterAction of
    Left _            -> put $ MoveWorker player
    Right _           -> put $ BuildUp player worker

  return boardAfterAction

buildUpT :: Player -> Worker -> Board -> GameStateT
buildUpT player worker board = do
  targetPosition <- readPosition $ "Select target position to build for " ++ show worker

  let boardAfterAction = buildUp worker targetPosition board

  case boardAfterAction of
    Left _            -> put $ BuildUp player worker
    Right _           -> put $ MoveWorker $ nextPlayer player

  return boardAfterAction

readPosition :: String -> BaseStateT Position
readPosition message = do
  positionInput <- readInput message
  return (read positionInput :: Position)

readWorker :: String -> BaseStateT Worker
readWorker message = do
  workerInput <- readInput message
  return (read workerInput :: Worker)

readInput :: String -> BaseStateT String
readInput message = do
  liftIO $ print message
  liftIO getLine
