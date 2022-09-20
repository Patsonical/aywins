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

User
  discordId ByteString
  totalWins Int

Win
  user UserId
  game GameId
  date UTCTime

|]
