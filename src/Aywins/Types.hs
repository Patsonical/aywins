{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE LambdaCase #-}

module Aywins.Types 
  ( SqlAction
  , ErrorMsg
  , Status
  , ScoreMod(..)
  , scoreModToFn
  ) where

import Control.Monad.IO.Class (MonadIO)
import Control.Monad.Trans.Reader (ReaderT)
import Database.Persist.Sqlite (SqlBackend)
import Data.Text (Text)

type SqlAction a = forall m. MonadIO m => ReaderT SqlBackend m a
type ErrorMsg = Text
type Status = Maybe ErrorMsg

data ScoreMod = Set Int | Inc Int | Dec Int
  deriving (Show, Eq)

scoreModToFn :: ScoreMod -> (Int -> Int)
scoreModToFn  = \case
  (Set n) -> const n
  (Inc n) -> (+ n)
  (Dec n) -> \x -> x - n
