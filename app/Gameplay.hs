{-# LANGUAGE StrictData #-}

module Gameplay
  ( playGame
  ) where

import Control.Monad.IO.Class (liftIO)
import Control.Monad.Trans.State (get, put, evalStateT)
import Prelude

import ReplUi
  ( clearScreen
  , displayBoard
  , displayBoardError
  , displayMessage
  , readWorker
  , readPosition
  )
import GameEngine
import Types (GameState (..) , GameStateT)

playGame :: IO ()
playGame = do
  let startGame = gameplayLoop emptyBoard
  let initialState = PlaceWorkers

  _ <- evalStateT startGame initialState

  return ()

gameplayLoop :: Board -> GameStateT
gameplayLoop board = do
  displayBoard board

  boardAfterTurn <- playTurn board

  clearScreen

  case boardAfterTurn of
    Left errorMessage -> do
      displayBoardError errorMessage
      gameplayLoop board
    Right newBoard -> do
      gameStateAfterAction <- get
      case gameStateAfterAction of
        WonGame winningPlayer -> do
          displayMessage $ show winningPlayer ++ " wins!"
          return boardAfterTurn
        _else ->
          gameplayLoop newBoard

playTurn :: Board -> GameStateT
playTurn board = do
  gameState <- get

  boardAfterAction <-
    case gameState of
      PlaceWorkers ->
        handlePlaceWorkersState board
      MoveWorker playerToMove ->
        handleMoveWorkerState playerToMove board
      BuildUp playerToBuild workerToBuild ->
        handleBuildUpState playerToBuild workerToBuild board
      WonGame _winningPlayer ->
        return $ Right board

  return boardAfterAction

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
      -- TODO: Add check for game won here and use `WonGame player`.
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
