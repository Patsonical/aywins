module Aywins.CommandParser where

import Aywins.Types (ScoreMod(..), scoreModToFn, Command (Lsgames))
import Aywins.Entities
import Data.Text (Text)

parseCommand :: Text -> Command
parseCommand _ = Lsgames
