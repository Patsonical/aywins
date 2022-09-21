{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE UndecidableInstances #-}
{-# OPTIONS_GHC -Wno-name-shadowing #-}

module Entities where

import Database.Persist.TH
import Data.Text ( Text )
import Data.Time ( UTCTime )
import Data.ByteString ( ByteString )

share [mkPersist sqlSettings, mkMigrate "migrateAll"] [persistLowerCase|

User
  discordId ByteString
  banned Bool default=False
  UniqueUserDiscordId discordId
  deriving Eq Show

Game
  name Text
  UniqueGameName name
  deriving Eq Show

GameAlias
  name Text
  link GameId
  UniqueGameAliasName name
  deriving Eq Show

Wins
  user UserId
  game GameId
  score Int default=0
  lastWinDate UTCTime Maybe
  UniqueWinUserGame user game
  deriving Eq Show

|]
