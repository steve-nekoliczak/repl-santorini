-- HACK: This is here to silence linter warnings.
{-# LANGUAGE OverloadedRecordDot #-}

module GameEngineSpec (spec) where

import Test.Hspec
import Data.Either
import Data.Map hiding (map)
import Prelude

import GameEngine
import Factory.BoardFactory

spec :: Spec
spec = do
  describe "emptyBoard" $ do
    it "returns a board with no buildings and no workers" $ do
      emptyBoard `shouldBe` emptyBoardFactory

  describe "spaceOnBoard" $ do
    let gridChanges =
          [ (Position (XA, Y1), Space LevelOne (Just IvoryMan))
          , (Position (XC, Y2), Space LevelThree (Just BlueWoman))
          , (Position (XC, Y4), Space LevelTwo Nothing)
          , (Position (XD, Y5), Space Dome Nothing)
          ]
    let modifiedBoard = modifyEmptyBoard gridChanges

    it "returns the space at the specified position" $ do
      spaceOnBoard (Position (XA, Y1)) modifiedBoard `shouldBe` Space LevelOne (Just IvoryMan)
      spaceOnBoard (Position (XC, Y2)) modifiedBoard `shouldBe` Space LevelThree (Just BlueWoman)
      spaceOnBoard (Position (XC, Y4)) modifiedBoard `shouldBe` Space LevelTwo Nothing
      spaceOnBoard (Position (XD, Y5)) modifiedBoard `shouldBe` Space Dome Nothing

  describe "buildUp" $ do
    it "builds on a non-dome space" $ do
      let position = Position (XC, Y4)
      let targetPosition = Position (XC, Y5)
      let placedWorkerBoard = placeWorker BlueMan position emptyBoardFactory
      let modifiedBoard = buildUp BlueMan targetPosition (fromRight emptyBoardFactory placedWorkerBoard)

      isRight placedWorkerBoard `shouldBe` True
      spaceOnBoard targetPosition (fromRight emptyBoardFactory modifiedBoard) `shouldBe` Space LevelOne Nothing

    it "returns an error when building on a dome" $ do
      let position = Position (XD, Y5)
      let targetPosition = Position (XE, Y5)
      let modifiedBoard = modifyEmptyBoard [(targetPosition, Space Dome Nothing)]
      let placedWorkerBoard = placeWorker BlueMan position modifiedBoard
      let errorBoard = buildUp BlueMan targetPosition (fromRight emptyBoardFactory placedWorkerBoard)

      isLeft errorBoard `shouldBe` True
      errorBoard `shouldBe` Left (BuildError "Can't build on top of a dome")

  describe "placeWorker" $ do
    it "puts a worker on the board and updates the board's grid map and workers map" $ do
      let position = Position (XC, Y4)
      let modifiedBoard = placeWorker BlueMan position emptyBoardFactory

      spaceOnBoard position (fromRight emptyBoardFactory modifiedBoard) `shouldBe` Space Ground (Just BlueMan)
      ((fromRight emptyBoardFactory modifiedBoard).workers) ! BlueMan `shouldBe` position

  describe "moveWorker" $ do
    it "moves a worker to another space of the same level" $ do
      let originPosition = Position (XC, Y4)
      let targetPosition = Position (XC, Y5)
      let modifiedBoard = placeWorker BlueMan originPosition emptyBoardFactory
      let boardAfterMove = moveWorker BlueMan targetPosition (fromRight emptyBoardFactory modifiedBoard)

      spaceOnBoard originPosition (fromRight emptyBoardFactory boardAfterMove) `shouldBe` Space Ground Nothing
      spaceOnBoard targetPosition (fromRight emptyBoardFactory boardAfterMove) `shouldBe` Space Ground (Just BlueMan)
      ((fromRight emptyBoardFactory boardAfterMove).workers) ! BlueMan `shouldBe` targetPosition

  describe "spaceIsAdjacent" $ do
    let targetSpaceNotAdjacentErrorMessage = "Target space needs to be adjacent to worker"

    it "returns board if the target space is one space away in the X plane" $ do
      let position = Position (XA, Y4)
      let modifiedBoard = placeWorker BlueMan position emptyBoardFactory
      let targetPosition = Position (XB, Y4)

      spaceIsAdjacent BlueMan targetPosition (fromRight emptyBoardFactory modifiedBoard) `shouldBe` modifiedBoard

    it "returns board if the target space is one space away in the Y plane" $ do
      let position = Position (XD, Y4)
      let modifiedBoard = placeWorker BlueMan position emptyBoardFactory
      let targetPosition = Position (XD, Y5)

      spaceIsAdjacent BlueMan targetPosition (fromRight emptyBoardFactory modifiedBoard) `shouldBe` modifiedBoard

    it "returns board if the target space is one space away both in the X place and Y plane" $ do
      let position = Position (XB, Y2)
      let modifiedBoard = placeWorker BlueMan position emptyBoardFactory
      let targetPosition = Position (XA, Y1)

      spaceIsAdjacent BlueMan targetPosition (fromRight emptyBoardFactory modifiedBoard) `shouldBe` modifiedBoard

    it "returns a TargetSpaceNotAdjacentError if the target space is not adjacent in the X plane" $ do
      let position = Position (XB, Y2)
      let modifiedBoard = placeWorker BlueMan position emptyBoardFactory
      let targetPosition = Position (XD, Y1)

      spaceIsAdjacent BlueMan targetPosition (fromRight emptyBoardFactory modifiedBoard) `shouldBe` Left (TargetSpaceNotAdjacentError targetSpaceNotAdjacentErrorMessage)

    it "returns a TargetSpaceNotAdjacentError if the target space is not adjacent in the Y plane" $ do
      let position = Position (XB, Y3)
      let modifiedBoard = placeWorker BlueMan position emptyBoardFactory
      let targetPosition = Position (XB, Y5)

      spaceIsAdjacent BlueMan targetPosition (fromRight emptyBoardFactory modifiedBoard) `shouldBe` Left (TargetSpaceNotAdjacentError targetSpaceNotAdjacentErrorMessage)

    it "returns a TargetSpaceNotAdjacentError if the target space is not adjacent in both the X plane and Y plane" $ do
      let position = Position (XC, Y3)
      let modifiedBoard = placeWorker BlueMan position emptyBoardFactory
      let targetPosition = Position (XE, Y5)

      spaceIsAdjacent BlueMan targetPosition (fromRight emptyBoardFactory modifiedBoard) `shouldBe` Left (TargetSpaceNotAdjacentError targetSpaceNotAdjacentErrorMessage)

    it "returns a WorkerNotYetPlacedError if the worker has not been placed yet" $ do
      let targetPosition = Position (XE, Y5)

      spaceIsAdjacent BlueMan targetPosition emptyBoardFactory `shouldBe` Left (WorkerNotYetPlacedError "Worker needs to be placed to check for adjacency")

