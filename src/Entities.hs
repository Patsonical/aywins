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
  totalWins Int
  deriving Eq Show

User
  discordId ByteString
  totalWins Int
  deriving Eq Show

Win
  user UserId
  game GameId
  date UTCTime
  deriving Eq Show

|]
