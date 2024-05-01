module ReplUiSpec
  ( spec
  ) where

import Test.Hspec
import Prelude

import ReplUi
import GameEngine

spec :: Spec
spec = do
  describe "boardString" $ do
    it "outputs the board as expected" $ do
      boardString emptyBoard `shouldBe`
        concat
          [ "|-------A------|-------B------|-------C------|-------D------|-------E------|\n"
          , "|              |              |              |              |              |\n"
          , "|              |              |              |              |              |\n"
          , "1              |              |              |              |              1\n"
          , "|              |              |              |              |              |\n"
          , "|              |              |              |              |              |\n"
          , "----------------------------------------------------------------------------\n"
          , "|              |              |              |              |              |\n"
          , "|              |              |              |              |              |\n"
          , "2              |              |              |              |              2\n"
          , "|              |              |              |              |              |\n"
          , "|              |              |              |              |              |\n"
          , "----------------------------------------------------------------------------\n"
          , "|              |              |              |              |              |\n"
          , "|              |              |              |              |              |\n"
          , "3              |              |              |              |              3\n"
          , "|              |              |              |              |              |\n"
          , "|              |              |              |              |              |\n"
          , "----------------------------------------------------------------------------\n"
          , "|              |              |              |              |              |\n"
          , "|              |              |              |              |              |\n"
          , "4              |              |              |              |              4\n"
          , "|              |              |              |              |              |\n"
          , "|              |              |              |              |              |\n"
          , "----------------------------------------------------------------------------\n"
          , "|              |              |              |              |              |\n"
          , "|              |              |              |              |              |\n"
          , "5              |              |              |              |              5\n"
          , "|              |              |              |              |              |\n"
          , "|              |              |              |              |              |\n"
          , "|-------A------|-------B------|-------C------|-------D------|-------E------|\n"
          ]
