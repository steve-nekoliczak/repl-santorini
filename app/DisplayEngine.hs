{-# LANGUAGE OverloadedRecordDot #-}

module DisplayEngine
  ( boardString
  ) where

import Data.List (intersperse)

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
  headerLine
  ++ rowLines
  ++ headerLine
    where
      rowLines = concat (intersperse borderLine rows)
      rows = map (\ y -> boardRow y board) yCoords

boardRow :: YCoord -> Board -> String
boardRow y board =
  blankLines 2
  ++ boardInfoLine y board
  ++ blankLines 2

widthPerSpace :: Int
widthPerSpace = 15

headerLine :: String
headerLine =
   headerSpaces ++ "|\n"
    where
      headerSpaces = concat . map (spaceInfoTemplate '-') $ xCoordChars
      -- TODO: This is the line that is failing the test.
      -- It's printing double quotes around the string.
      xCoordChars = map show $ xCoords

borderLine :: String
borderLine =
  (take tableWidth . cycle $ "-") ++ "\n"

blankLines :: Int -> String
blankLines n =
  concat . replicate n $ blankLine

blankLine :: String
blankLine =
  (take tableWidth . cycle $ "|" ++ blankSpaces) ++ "\n"
    where
      blankSpaces = take (widthPerSpace - 1) $ cycle " "

spaceInfoTemplate :: Char -> String -> String
spaceInfoTemplate filler string =
  "|"
  ++ (take (widthPerSpace `div` 2) $ cycle [filler])
  ++ string
  ++ (take ((widthPerSpace `div` 2) - 1) $ cycle [filler])

spaceInfo :: Position -> Board -> String
spaceInfo position board=
  spaceInfoTemplate ' ' [workerChar]
    where
      space = spaceOnBoard position board
      workerChar =
        case space.worker of
          Nothing     -> ' '
          -- TODO: Format worker output character.
          -- Just worker -> show worker
          Just worker -> 'A'

boardInfoLine :: YCoord -> Board -> String
boardInfoLine y board =
  concat cells ++ "|\n"
    where
      cells = map (\ x -> spaceInfo (Position(x, y)) board) xCoords

tableWidth :: Int
tableWidth =
  (widthPerSpace * xSpacesCount) + 1
    where xSpacesCount = length xCoords
