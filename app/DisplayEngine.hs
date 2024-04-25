{-# LANGUAGE OverloadedRecordDot #-}

module DisplayEngine
  ( boardString
  , borderLine
  ) where

import Data.Map ((!))

import GameEngine
  ( Board (..)
  , Position (..)
  , Space (..)
  , Worker (..)
  , XCoord (..)
  , YCoord (..)
  , spaceOnBoard
  , xCoords
  , yCoords
  )

boardString :: Board -> String
boardString board =
  borderLine
  ++ blankLine
  ++ blankLine
  ++ boardInfoLine Y5 board

widthPerSpace :: Int
widthPerSpace = 15

heightPerSpace :: Int
heightPerSpace = 7

borderLine :: String
borderLine =
  (take tableWidth . cycle $ "-") ++ "\n"

blankLine :: String
blankLine =
  (take tableWidth . cycle $ "|" ++ blankSpaces) ++ "\n"
    where
      blankSpaces = take (widthPerSpace - 1) $ cycle " "

cell :: Position -> Board -> String
cell position board=
  "|"
  ++ (take (widthPerSpace `div` 2) $ cycle " ")
  ++ workerChar
  ++ (take ((widthPerSpace `div` 2) - 1) $ cycle " ")
    where
      space = spaceOnBoard position board
      workerChar =
        case space.worker of
          Nothing     -> " "
          -- TODO: Format worker output character.
          Just worker -> show worker

boardInfoLine :: YCoord -> Board -> String
boardInfoLine y board =
  concat cells ++ "|\n"
    where
      cells = map (\ x -> cell (Position(x, y)) board) xCoords

tableWidth :: Int
tableWidth =
  (widthPerSpace * xSpacesCount) + 1
    where xSpacesCount = length xCoords

tableHeight :: Int
tableHeight =
  (heightPerSpace * ySpacesCount) + 1
    where ySpacesCount = length xCoords
