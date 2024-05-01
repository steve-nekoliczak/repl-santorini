{-# LANGUAGE OverloadedRecordDot #-}

module ReplUi
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
  , xCoordStrings
  , yCoords
  , yCoordString
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
   "|" ++ headerSpaces ++ "|\n"
    where
      headerSpaces = concat . intersperse "|" $ spaces
        where spaces = map (infoSpaceTemplate '-') $ xCoordStrings

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

infoSpaceTemplate :: Char -> String -> String
infoSpaceTemplate filler string =
  (take (widthPerSpace `div` 2) $ cycle [filler])
  ++ string
  ++ (take ((widthPerSpace `div` 2) - 1) $ cycle [filler])

infoSpace :: Position -> Board -> String
infoSpace position board=
  infoSpaceTemplate ' ' workerString
    where
      space = spaceOnBoard position board
      workerString =
        case space.worker of
          Nothing     -> " "
          -- TODO: Format worker output character.
          -- Just worker -> show worker
          Just worker -> "A"

boardInfoLine :: YCoord -> Board -> String
boardInfoLine y board =
   yCoordString y ++ boardSpaces ++ yCoordString y ++ "\n"
    where
      boardSpaces = (concat . intersperse "|" $ cells)
      cells = map (\ x -> infoSpace (Position(x, y)) board) xCoords

tableWidth :: Int
tableWidth =
  (widthPerSpace * xSpacesCount) + 1
    where xSpacesCount = length xCoords
