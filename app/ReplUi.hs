{-# LANGUAGE OverloadedRecordDot #-}

module ReplUi
  ( boardLines
  ) where

import Data.List (intersperse)

import GameEngine
  ( Board (..)
  , Position (..)
  , Space (..)
  , YCoord (..)
  , spaceOnBoard
  , xCoords
  , xCoordStrings
  , yCoords
  , yCoordString
  )

-- *Lines - String
-- A concatenation of individual lines.
--
-- *Line - String
-- A single line of output.
--
-- *Fragments - [String]
-- A list of fragments used to build a line.
--
-- *Fragment - String
-- A fragment of a single line of output.

boardLines :: Board -> String
boardLines board =
  headerLine
  ++ rows
  ++ headerLine
    where
      rows = concat . intersperse borderLine $ rowLinesList
      rowLinesList = map (\ y -> rowLines y board) (reverse yCoords)

rowLines :: YCoord -> Board -> String
rowLines y board =
  blankLines 2
  ++ infoLine y board
  ++ blankLines 2

widthPerSpace :: Int
widthPerSpace = 15

headerLine :: String
headerLine =
   "|" ++ headerSpaces ++ "|\n"
    where
      headerSpaces = spacesLine "|" '-'  xCoordStrings

borderLine :: String
borderLine =
   "|" ++ borderSpaces ++ "|\n"
    where
      borderSpaces = spacesLine "|" '-' hyphens
      hyphens = take (length xCoords) . cycle $ ["-"]

spacesLine :: String -> Char -> [String] -> String
spacesLine border filler strings =
  concat . intersperse border $ spaceFragments filler strings

spaceFragments :: Char -> [String] -> [String]
spaceFragments filler strings =
  map (spaceFragment filler) $ strings

spaceFragment :: Char -> String -> String
spaceFragment filler string =
  (take (widthPerSpace `div` 2) $ cycle [filler])
  ++ string
  ++ (take ((widthPerSpace `div` 2) - 1) $ cycle [filler])

blankLines :: Int -> String
blankLines n =
  concat . replicate n $ blankLine

blankLine :: String
blankLine =
   "|" ++ blankSpaces ++ "|\n"
    where
      blankSpaces = spacesLine "|" ' ' spaces
      spaces = take (length xCoords) . cycle $ [" "]

workerAtPositionString :: Position -> Board -> String
workerAtPositionString position board =
  workerString
    where
      space = spaceOnBoard position board
      workerString =
        case space.worker of
          Nothing     -> " "
          -- TODO: Format worker output character.
          -- Just worker -> show worker
          Just worker -> "A"

infoLine :: YCoord -> Board -> String
infoLine y board =
   yCoordString y ++ boardSpaces ++ yCoordString y ++ "\n"
    where
      boardSpaces = spacesLine "|" ' ' workerStrings
      workerStrings = map (\ x -> workerAtPositionString (Position(x, y)) board) xCoords
