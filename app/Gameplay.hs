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
  let startGame = gameplayLoopT emptyBoard
  let initialState = PlaceWorkers
  newGame <- runStateT startGame initialState
  print $ fst newGame
  return ()

gameplayLoopT :: Board -> GameStateT
gameplayLoopT board = do
  liftIO $ putStrLn $ boardLines board
  liftIO $ putStrLn "-----"

  state' <- get

  boardAfterAction <-
        case state' of
          PlaceWorkers -> do
            targetPosition <- case nextWorkerToPlace board of
              Just workerToPlace ->
                readPosition $ "Please place " ++ show workerToPlace ++ " character"
              Nothing ->
                return Nothing

            case targetPosition of
              Just position -> do
                placeNextWorkerT position board
              Nothing ->
                return (Left $ AllWorkersPlacedError "All workers have been placed.")

          MoveWorker playerToMove           -> do
            workerToMove    <- readWorker $ "Select a character for " ++ show playerToMove ++ ": " ++ show (workersForPlayer playerToMove)
            targetPosition  <- readPosition $ "Select target position for " ++ show workerToMove

            case (workerToMove, targetPosition) of
              (Just workerToMove', Just targetPosition') -> do
                moveWorkerT playerToMove workerToMove' targetPosition' board
              (Nothing, _) ->
                return (Left $ InvalidWorkerError "Please select a valid worker.")
              (Just _, Nothing) ->
                return (Left $ InvalidPositionError "Please select a valid position.")

          BuildUp targetPlayer targetWorker -> do
            targetPosition <- readPosition $ "Select target position to build for " ++ show targetWorker

            case targetPosition of
              Just position -> do
                buildUpT targetPlayer targetWorker position board
              Nothing ->
                return (Left $ InvalidPositionError "Please select a valid position.")

          GameOver ->
            return $ Right board

  stateAfterAction <- get

  liftIO $ clearScreen
  case boardAfterAction of
    Left errorMessage   ->
      liftIO (print errorMessage) >> gameplayLoopT board
    Right newBoard      ->
      case stateAfterAction of
        GameOver        -> return boardAfterAction
        _else           -> gameplayLoopT newBoard

placeNextWorkerT :: Position -> Board -> GameStateT
placeNextWorkerT targetPosition board = do
  let boardAfterAction = placeNextWorker targetPosition board

  case boardAfterAction of
    Left _            -> put PlaceWorkers
    Right newBoard    ->
      case nextWorkerToPlace newBoard of
        Nothing       -> put $ MoveWorker BluePlayer
        Just _        -> put PlaceWorkers

  return boardAfterAction

moveWorkerT :: Player -> Worker -> Position -> Board -> GameStateT
moveWorkerT playerToMove workerToMove targetPosition board = do
  let boardAfterAction = moveWorker workerToMove targetPosition board

  case boardAfterAction of
    Left _            -> put $ MoveWorker playerToMove
    Right _           -> put $ BuildUp playerToMove workerToMove

  return boardAfterAction

buildUpT :: Player -> Worker -> Position -> Board -> GameStateT
buildUpT playerToBuild workerToBuild targetPosition board = do
  let boardAfterAction = buildUp workerToBuild targetPosition board

  case boardAfterAction of
    Left _            -> put $ BuildUp playerToBuild workerToBuild
    Right _           -> put $ MoveWorker $ nextPlayer playerToBuild

  return boardAfterAction
