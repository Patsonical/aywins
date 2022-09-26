{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE LambdaCase #-}

module Aywins.Types where

import Control.Monad.IO.Class (MonadIO)
import Control.Monad.Trans.Reader (ReaderT)
import Database.Persist.Sqlite (SqlBackend)
import Data.Text (Text, pack)
import qualified Discord.Types as D
import Database.Persist (Entity)
import Aywins.Entities (User, Game)
import Data.ByteString (ByteString)

type SqlAction a = forall m. MonadIO m => ReaderT SqlBackend m a
type ErrorMsg = Text
type SqliteConnStr = Text

data Status = Success | NotImpl | Error ErrorMsg | Warning ErrorMsg | Message Text

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
  | Setscore (Maybe D.User) ScoreMod Text
  | Amiwinning (Maybe Text)
  | Aretheywinning D.User (Maybe Text)
  | Whoiswinning (Maybe Text)
  | Rmself
  | AywinsHelp
  | Theywon D.User Text
  | Addgame Text
  | Rmgame Text
  | Adduser D.User
  | Rmuser D.User
  | Banuser D.User
  | Unbanuser D.User
  | Mergegames [Text]
  | Renamegame Text Text

data Response = SingleUserResponse      D.User [(Text, Int)]
              | ScoreResponse           D.User Text Int
              | GameLeaderboardResponse Text [(ByteString, Int)]
              | FullLeaderboardResponse [(Text, [(ByteString, Int)])]

fmtResponse :: Response -> Text
fmtResponse = pack . \case
  SingleUserResponse      user gameScores -> ""
  ScoreResponse           user game score -> ""
  GameLeaderboardResponse game userScores -> ""
  FullLeaderboardResponse leaderboard     -> ""
