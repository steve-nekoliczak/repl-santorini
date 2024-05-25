{-# LANGUAGE StrictData #-}

module Gameplay
  ( main
  ) where

import System.Console.ANSI (clearScreen)
import Control.Monad.IO.Class (liftIO)
import Control.Monad.Trans.State (get, put, runStateT)
import Prelude

import ReplUi (boardLines, readWorker, readPosition)
import GameEngine
import Types (GameState (..) , GameStateT)

main :: IO ()
main = do
  putStrLn "Starting new game of Santorini!"
  let startGame = gameplayLoop emptyBoard
  let initialState = PlaceWorkers
  newGame <- runStateT startGame initialState
  print $ fst newGame
  return ()

gameplayLoop :: Board -> GameStateT
gameplayLoop board = do
  liftIO $ putStrLn $ boardLines board
  liftIO $ putStrLn "-----"

  state' <- get

  boardAfterAction <-
    case state' of
      PlaceWorkers ->
        handlePlaceWorkersState board
      MoveWorker playerToMove ->
        handleMoveWorkerState playerToMove board
      BuildUp playerToBuild workerToBuild ->
        handleBuildUpState playerToBuild workerToBuild board
      GameOver ->
        return $ Right board

  stateAfterAction <- get

  liftIO $ clearScreen

  case boardAfterAction of
    Left errorMessage ->
      liftIO (print errorMessage) >> gameplayLoop board
    Right newBoard ->
      case stateAfterAction of
        GameOver ->
          return boardAfterAction
        _else ->
          gameplayLoop newBoard

handlePlaceWorkersState :: Board -> GameStateT
handlePlaceWorkersState board = do
  targetPosition <-
    case nextWorkerToPlace board of
      Just workerToPlace ->
        readPosition $ "Please place " ++ show workerToPlace ++ " character"
      Nothing ->
        return Nothing

  case targetPosition of
    Just position ->
      placeNextWorkerInGame position board
    Nothing ->
      return (Left $ AllWorkersPlacedError "All workers have been placed.")

placeNextWorkerInGame :: Position -> Board -> GameStateT
placeNextWorkerInGame targetPosition board = do
  let boardAfterAction = placeNextWorker targetPosition board

  case boardAfterAction of
    Left _ ->
      put PlaceWorkers
    Right newBoard ->
      case nextWorkerToPlace newBoard of
        Nothing ->
          put $ MoveWorker BluePlayer
        Just _ ->
          put PlaceWorkers

  return boardAfterAction

handleMoveWorkerState :: Player -> Board -> GameStateT
handleMoveWorkerState playerToMove board = do
  workerToMove    <- readWorker $ "Select a character for " ++ show playerToMove ++ ": " ++ show (workersForPlayer playerToMove)
  targetPosition  <- readPosition $ "Select target position for " ++ show workerToMove

  case (workerToMove, targetPosition) of
    (Just workerToMove', Just targetPosition') ->
      moveWorkerInGame playerToMove workerToMove' targetPosition' board
    (Nothing, _) ->
      return (Left $ InvalidWorkerError "Please select a valid worker.")
    (Just _, Nothing) ->
      return (Left $ InvalidPositionError "Please select a valid position.")

moveWorkerInGame :: Player -> Worker -> Position -> Board -> GameStateT
moveWorkerInGame playerToMove workerToMove targetPosition board = do
  let boardAfterAction = moveWorker workerToMove targetPosition board

  case boardAfterAction of
    Left _ ->
      put $ MoveWorker playerToMove
    Right _ ->
      put $ BuildUp playerToMove workerToMove

  return boardAfterAction

handleBuildUpState :: Player -> Worker -> Board -> GameStateT
handleBuildUpState playerToBuild workerToBuild board = do
  targetPosition <- readPosition $ "Select target position to build for " ++ show workerToBuild

  case targetPosition of
    Just position ->
      buildUpInGame playerToBuild workerToBuild position board
    Nothing ->
      return (Left $ InvalidPositionError "Please select a valid position.")

buildUpInGame :: Player -> Worker -> Position -> Board -> GameStateT
buildUpInGame playerToBuild workerToBuild targetPosition board = do
  let boardAfterAction = buildUp workerToBuild targetPosition board

  case boardAfterAction of
    Left _ ->
      put $ BuildUp playerToBuild workerToBuild
    Right _ ->
      put $ MoveWorker $ nextPlayer playerToBuild

  return boardAfterAction
