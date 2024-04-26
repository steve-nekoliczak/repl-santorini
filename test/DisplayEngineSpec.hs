module DisplayEngineSpec
  ( spec
  ) where

import Test.Hspec
import Prelude

import DisplayEngine
import GameEngine

spec :: Spec
spec = do
  describe "boardString" $ do
    it "outputs the board as expected" $ do
      boardString emptyBoard `shouldBe`
        concat
          [ "----------------------------------------------------------------------------\n"
          , "|              |              |              |              |              |\n"
          , "|              |              |              |              |              |\n"
          , "|              |              |              |              |              |\n"
          ]
