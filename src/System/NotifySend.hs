{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell   #-}

module System.NotifySend (
    defaultNotification,
    notifySend,
    summary,
    body,
    expireTime,
    urgency,
    icon,
    Command,
    commandOptions
  ) where

import           Control.Lens    hiding (elements)
import           Data.Char
import           Data.Text       hiding (map, toLower)
import qualified Data.Text       as T
import           Shelly
import           Test.QuickCheck

data Urgency = Low | Normal | Critical deriving (Show, Eq)

data Command = Command {
  _summary    :: Text,
  _body       :: Text,
  _expireTime :: Int,
  _urgency    :: Urgency,
  _icon       :: Prelude.FilePath
} deriving (Show, Eq)

genText :: Gen Text
genText = fmap pack $ listOf (elements ['a' .. 'z'])

instance Arbitrary Command where
  arbitrary = do
    s <- genText
    b <- genText
    e <- arbitrary
    u <- elements [Low, Normal, Critical]
    i <- fmap show genText
    return $ Command s b e u i

$(makeLenses ''Command)

type ShellyOptions = [Text]

commandOptions :: Command -> ShellyOptions
commandOptions c = ["-u", (T.toLower . pack . show) $ c ^. urgency, "-t", (pack . show) $ max 0 (c ^. expireTime), "-i", pack $ c ^. icon]

-- | A default notification, with no message. You can modify the defaults using the Lens API
--
-- >>>  notifySend $ defaultNotification & body .~ "hello world"
defaultNotification :: Command
defaultNotification = Command blank blank 1000 Low " "
  where blank = " " :: Text

-- | Send the notification via the notify-send option. This no-ops if notify-send is not found.
notifySend command = shelly $ verbosely $ do
  path <- which name
  case path of
    Just _ -> notify_send (commandOptions command) (command ^. summary) (command ^. body)
    Nothing -> return ""
  where notify_send opts s b = run name (opts ++ [s,b])
        name = "notify-send"
