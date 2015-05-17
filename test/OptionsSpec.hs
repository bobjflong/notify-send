{-# LANGUAGE OverloadedStrings #-}

module OptionsSpec(spec) where

import           Control.Lens
import           System.NotifySend
import           Test.Hspec
import           Test.QuickCheck
import Data.Text

spec :: Spec
spec = do
  describe "Command line options to notify-send"  $ do
    it "should encode correctly" $ do
      property $ \x -> (commandOptions x) `shouldBe` ["-u", (Data.Text.toLower . pack. show) $ x ^. urgency, "-t", (pack . show) $ max 0 (x ^. expireTime), "-i", pack $ x ^. icon]
    it "should default correctly" $ do
      (commandOptions defaultNotification) `shouldBe` ["-u", "low", "-t", "1000", "-i", " "]
