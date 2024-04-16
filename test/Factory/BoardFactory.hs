{-# LANGUAGE OverloadedRecordDot #-}

module Factory.BoardFactory (emptyBoardFactory, modifyEmptyBoard) where

import Data.Map
import Prelude

import GameEngine
import Helper.Map

emptyBoardFactory :: Board
emptyBoardFactory = Board emptyGrid unplacedWorkers

emptyGrid :: Map Position Space
emptyGrid = fromList [ (Position (XA, Y1), Space Ground Nothing)
                     , (Position (XA, Y2), Space Ground Nothing)
                     , (Position (XA, Y3), Space Ground Nothing)
                     , (Position (XA, Y4), Space Ground Nothing)
                     , (Position (XA, Y5), Space Ground Nothing)
                     , (Position (XB, Y1), Space Ground Nothing)
                     , (Position (XB, Y2), Space Ground Nothing)
                     , (Position (XB, Y3), Space Ground Nothing)
                     , (Position (XB, Y4), Space Ground Nothing)
                     , (Position (XB, Y5), Space Ground Nothing)
                     , (Position (XC, Y1), Space Ground Nothing)
                     , (Position (XC, Y2), Space Ground Nothing)
                     , (Position (XC, Y3), Space Ground Nothing)
                     , (Position (XC, Y4), Space Ground Nothing)
                     , (Position (XC, Y5), Space Ground Nothing)
                     , (Position (XD, Y1), Space Ground Nothing)
                     , (Position (XD, Y2), Space Ground Nothing)
                     , (Position (XD, Y3), Space Ground Nothing)
                     , (Position (XD, Y4), Space Ground Nothing)
                     , (Position (XD, Y5), Space Ground Nothing)
                     , (Position (XE, Y1), Space Ground Nothing)
                     , (Position (XE, Y2), Space Ground Nothing)
                     , (Position (XE, Y3), Space Ground Nothing)
                     , (Position (XE, Y4), Space Ground Nothing)
                     , (Position (XE, Y5), Space Ground Nothing)
                     ]

unplacedWorkers :: Map Worker Position
unplacedWorkers = fromList [ (BlueMan, NotOnBoard)
                           , (BlueWoman, NotOnBoard)
                           , (IvoryMan, NotOnBoard)
                           , (IvoryWoman, NotOnBoard)
                           ]

modifyEmptyBoard :: [(Position, Space)] -> Board
modifyEmptyBoard gridChanges = emptyBoardFactory { grid = modifiedGrid }
  where modifiedGrid = insertMany gridChanges (emptyBoardFactory.grid)
