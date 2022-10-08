module Aywins.CommandParser where

import Aywins.Types (Command (..))
import Discord.Interactions (OptionsData)
import Data.Text (Text)

parseCommand :: Text -> OptionsData -> Maybe Command
parseCommand _ _ = Just Lsgames
