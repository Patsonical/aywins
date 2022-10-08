{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE LambdaCase #-}

module Aywins.CommandParser where

import Aywins.Types
import Aywins.Lib
import Discord.Interactions (OptionDataValue (..))
import Data.Text (Text)
import qualified Data.Text as T
import Data.List (find)
import Control.Monad.Trans.Reader
import Control.Monad.Trans.Class (MonadTrans(..))
import qualified Discord.Types as D
import Text.Read (readMaybe)
import Data.Maybe (catMaybes)
import Data.List.NonEmpty (NonEmpty(..))

isOptStr, isOptUser :: OptionDataValue -> Bool
isOptStr = \case
  OptionDataValueString _ _ -> True
  _                         -> False
isOptUser = \case
  OptionDataValueUser _ _ -> True
  _                       -> False

getOptStr :: Text -> ReaderT [OptionDataValue] Maybe Text
getOptStr name = do
  option <- lift . find (\o -> isOptStr o && optionDataValueName o == name) =<< ask
  lift . rightToMaybe $ optionDataValueString option

getOptUser :: Text -> ReaderT [OptionDataValue] Maybe D.UserId
getOptUser name = do
  option <- lift . find (\o -> isOptUser o && optionDataValueName o == name) =<< ask
  pure $ optionDataValueUser option

maybeOpt :: (Text -> ReaderT [OptionDataValue] Maybe a) -> Text -> ReaderT [OptionDataValue] Maybe (Maybe a)
maybeOpt f t = flip mapReaderT (f t) $ \case
  Nothing -> Just Nothing
  Just x  -> Just (Just x)

parseScoreMod :: Text -> Maybe ScoreMod
parseScoreMod raw = let readMaybeText = readMaybe . T.unpack in
  T.uncons raw >>= \case
    ('+', xs) -> Inc <$> readMaybeText xs
    ('-', xs) -> Dec <$> readMaybeText xs
    _         -> Set <$> readMaybeText raw

parseCommand :: Text -> [OptionDataValue] -> Maybe Command
parseCommand cmd options = flip runReaderT options $ case cmd of
  "iwon"           -> Iwon <$> getOptStr "game"
  "setscore"       -> do userId   <- maybeOpt getOptUser "target"
                         scoreMod <- lift . parseScoreMod =<< getOptStr "scoremod"
                         gameName <- getOptStr "game"
                         pure $ Setscore userId scoreMod gameName
  "amiwinning"     -> Amiwinning <$> maybeOpt getOptStr "game"
  "aretheywinning" -> Aretheywinning <$> getOptUser "target" <*> maybeOpt getOptStr "game"
  "whoiswinning"   -> Whoiswinning <$> maybeOpt getOptStr "game"
  "rmself"         -> pure Rmself
  "lsgames"        -> pure Lsgames
  "aywinshelp"     -> pure AywinsHelp
  "theywon"        -> Theywon <$> getOptUser "target" <*> getOptStr "game"
  "addgame"        -> Addgame <$> getOptStr "game"
  "rmgame"         -> Rmgame <$> getOptStr "game"
  "adduser"        -> Adduser <$> getOptUser "user"
  "rmuser"         -> Rmuser <$> getOptUser "user"
  "banuser"        -> Banuser <$> getOptUser "user"
  "unbanuser"      -> Unbanuser <$> getOptUser "user"
  "mergegames"     -> do targetGame <- getOptStr "targetgame"
                         mergeGames <- sequence [
                             Just <$> getOptStr "mergegame1"
                           , maybeOpt getOptStr "mergegame2"
                           , maybeOpt getOptStr "mergegame3"
                           , maybeOpt getOptStr "mergegame4"
                           ]
                         pure $ Mergegames (targetGame :| catMaybes mergeGames)
  "renamegame"     -> Renamegame <$> getOptStr "oldname" <*> getOptStr "newName"
  _                -> lift Nothing
