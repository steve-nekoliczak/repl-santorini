module Gameplay
  ( main
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
          PlaceWorkers                      -> placeNextWorkerT board
          MoveWorker targetPlayer           -> moveWorkerT targetPlayer board
          BuildUp targetPlayer targetWorker -> buildUpT targetPlayer targetWorker board
          GameOver                          -> return $ Right board

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
    Just workerToPlace -> do
      readPosition $ "Please place " ++ show workerToPlace ++ " character"
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
moveWorkerT playerToMove board = do
  workerToMove <- readWorker $ "Select a character for " ++ show playerToMove ++ ": " ++ show (workersForPlayer playerToMove)
  targetPosition <- readPosition $ "Select target position for " ++ show workerToMove

  let boardAfterAction = moveWorker workerToMove targetPosition board

  case boardAfterAction of
    Left _            -> put $ MoveWorker playerToMove
    Right _           -> put $ BuildUp playerToMove workerToMove

  return boardAfterAction

buildUpT :: Player -> Worker -> Board -> GameStateT
buildUpT playerToBuild workerToBuild board = do
  targetPosition <- readPosition $ "Select target position to build for " ++ show workerToBuild

  let boardAfterAction = buildUp workerToBuild targetPosition board

  case boardAfterAction of
    Left _            -> put $ BuildUp playerToBuild workerToBuild
    Right _           -> put $ MoveWorker $ nextPlayer playerToBuild

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
