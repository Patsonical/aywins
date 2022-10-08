{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE LambdaCase #-}

module Aywins.Types where

import Control.Monad.IO.Class (MonadIO)
import Control.Monad.Trans.Reader (ReaderT)
import Database.Persist.Sqlite (SqlBackend)
import Data.Text (Text)
import qualified Discord.Types as D
import Data.ByteString (ByteString)
import Data.List.NonEmpty

type SqlAction a = forall m. MonadIO m => ReaderT SqlBackend m a
type SqliteConnStr = Text

data Error = NotImplError
           | AdminRoleMissingError
           | MergeTargetsMissingError
           | ParseFailureError
           | GameExistsError
           | GameMissingError
           | UserNotAdminError
           | UserExistsError
           | UserMissingError
           | UserBannedError
           | OtherError Text
              deriving (Show, Eq)

data Response = SingleUserResponse      D.UserId [(Text, Int)]
              | ScoreResponse           D.UserId Text Int
              | GameLeaderboardResponse Text [(ByteString, Int)]
              | FullLeaderboardResponse [(Text, [(ByteString, Int)])]
              | GamesListResponse       [Text]
                  deriving (Show, Eq)

data Status = Success | Failure Error | Warning Text | Message Text | Reply Response
  deriving (Show, Eq)

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
  | Setscore (Maybe D.UserId) ScoreMod Text
  | Amiwinning (Maybe Text)
  | Aretheywinning D.UserId (Maybe Text)
  | Whoiswinning (Maybe Text)
  | Rmself
  | Lsgames
  | AywinsHelp
  | Theywon D.UserId Text
  | Addgame Text
  | Rmgame Text
  | Adduser D.UserId
  | Rmuser D.UserId
  | Banuser D.UserId
  | Unbanuser D.UserId
  | Mergegames (NonEmpty Text)
  | Renamegame Text Text
