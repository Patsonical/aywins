{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE LambdaCase #-}

module Aywins.Types 
  ( SqlAction
  , ErrorMsg
  , Status
  , ScoreMod(..)
  , scoreModToFn
  , Command(..)
  ) where

import Control.Monad.IO.Class (MonadIO)
import Control.Monad.Trans.Reader (ReaderT)
import Database.Persist.Sqlite (SqlBackend)
import Data.Text (Text)
import Discord.Types (UserId)

type SqlAction a = forall m. MonadIO m => ReaderT SqlBackend m a
type ErrorMsg = Text
type SqliteConnStr = Text
type Status = Maybe ErrorMsg

data ScoreMod = Set Int | Inc Int | Dec Int
  deriving (Show, Eq)

scoreModToFn :: ScoreMod -> (Int -> Int)
scoreModToFn  = \case
  (Set n) -> const n
  (Inc n) -> (+ n)
  (Dec n) -> \x -> x - n

-- Raw input commands will be parsed into this
data Command = 
    Iwon Text
  | Setscore (Maybe UserId) ScoreMod Text
  | Amiwinning (Maybe Text)
  | Aretheywinning UserId (Maybe Text)
  | Whoiswinning (Maybe Text)
  | Rmself
  | AywinsHelp
  | Theywon UserId Text
  | Addgame Text
  | Rmgame Text
  | Adduser UserId
  | Rmuser UserId
  | Banuser UserId
  | Unbanuser UserId
  | Mergegames [Text]
  | Renamegame Text Text
