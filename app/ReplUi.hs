{-# LANGUAGE OverloadedRecordDot #-}

module ReplUi
  ( boardLines
  , clearScreen
  , displayBoard
  , displayBoardError
  , readPosition
  , readWorker
  , readInput
  ) where

import Control.Monad.IO.Class (liftIO)
import Data.List (intersperse)
import qualified Slist as SL
import qualified System.Console.ANSI as ANSI
import Text.Read (readMaybe)

import GameEngine
  ( spaceOnBoard
  , xCoords
  , xCoordStrings
  , yCoords
  , yCoordString
  )

import Types
  ( Board (..)
  , BoardError (..)
  , Position (..)
  , Space (..)
  , Worker (..)
  , YCoord (..)
  , BaseStateT
  )

widthPerSpace :: Int
widthPerSpace = 15

displayBoard :: Board -> BaseStateT ()
displayBoard board = do
  liftIO $ putStrLn $ boardLines board
  liftIO $ putStrLn "-----"

displayBoardError :: BoardError -> BaseStateT ()
displayBoardError boardError =
  liftIO $ print boardError

clearScreen :: BaseStateT ()
clearScreen = do
  liftIO $ ANSI.clearScreen

--
-- *Lines Functions
-- Functions that return a concatenation of individual lines.
--

boardLines :: Board -> String
boardLines board =
  headerLine
  ++ rowsLines
  ++ headerLine
    where
      rowsLines = concat . intersperse borderLine $ rowsLinesList
      rowsLinesList = map (\ y -> rowLines y board) (reverse yCoords)

rowLines :: YCoord -> Board -> String
rowLines y board =
  blankLines 2
  ++ infoLine y board
  ++ blankLines 2

blankLines :: Int -> String
blankLines n =
  concat . replicate n $ blankLine

--
-- *Line Functions
-- Functions that return a single line of output.
--

headerLine :: String
headerLine =
   "|" ++ headerSpaces ++ "|\n"
    where
      headerSpaces = lineTemplate "|" '-'  xCoordStrings

borderLine :: String
borderLine =
   "|" ++ borderSpaces ++ "|\n"
    where
      borderSpaces = lineTemplate "|" '-' hyphens
      hyphens = replicate (length $ SL.slist xCoords) "-"

-- Line that displays:
-- 1. The YCoord of the line in the border.
-- 2. The level of each space in the line.
-- 3. The worker of each space in the line (if a worker occupies the space).
infoLine :: YCoord -> Board -> String
infoLine y board =
   yCoordString y ++ boardSpaces ++ yCoordString y ++ "\n"
    where
      boardSpaces = lineTemplate "|" ' ' workerStrings
      workerStrings = map (\ x -> workerAtPositionString (Position(x, y)) board) xCoords

blankLine :: String
blankLine =
   "|" ++ blankSpaces ++ "|\n"
    where
      blankSpaces = lineTemplate "|" ' ' spaces
      spaces = replicate (length $ SL.slist xCoords) " "

--
-- Template Functions
--

lineTemplate :: String -> Char -> [String] -> String
lineTemplate border filler strings =
  concat . intersperse border $ fragmentTemplates filler strings

fragmentTemplates :: Char -> [String] -> [String]
fragmentTemplates filler strings =
  map (fragmentTemplate filler) $ strings

fragmentTemplate :: Char -> String -> String
fragmentTemplate filler string =
  (replicate (widthPerSpace `div` 2) filler)
  ++ string
  ++ (replicate ((widthPerSpace `div` 2) - 1) filler)

--
-- Other Helper Functions
--

workerAtPositionString :: Position -> Board -> String
workerAtPositionString position board =
  case space.worker of
    Nothing             -> " "
    Just workerAtSpace  -> workerString workerAtSpace
    where
      space = spaceOnBoard position board

workerString :: Worker -> String
workerString BlueMan = "M"
workerString BlueWoman = "W"
workerString IvoryMan = "m"
workerString IvoryWoman = "w"

--
-- Input Functions
--

readPosition :: String -> BaseStateT (Maybe Position)
readPosition message = do
  positionInput <- readInput message
  return (readMaybe positionInput :: Maybe Position)

readWorker :: String -> BaseStateT (Maybe Worker)
readWorker message = do
  workerInput <- readInput message
  return (readMaybe workerInput :: Maybe Worker)

readInput :: String -> BaseStateT String
readInput message = do
  liftIO $ print message
  liftIO getLine
