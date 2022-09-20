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

Game
  name Text
  totalWins Int default=0
  UniqueGameName name
  deriving Eq Show

User
  discordId ByteString
  totalWins Int default=0
  banned Bool default=False
  UniqueDiscordId discordId
  deriving Eq Show

Win
  user UserId
  game GameId
  date UTCTime default=CURRENT_TIME
  deriving Eq Show

SetScore
  user UserId
  game GameId
  date UTCTime default=CURRENT_TIME
  val Int
  deriving Eq Show

|]
