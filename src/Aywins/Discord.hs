{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE NamedFieldPuns #-}

module Aywins.Discord where

import Aywins.Lib
import Control.Concurrent.MVar (readMVar)
import Control.Monad.IO.Class (MonadIO(..))
import Control.Monad.Trans.Reader (asks)
import Data.Function ((&))
import Data.Text (Text)
import Discord
import Discord.Handle (DiscordHandle(..))
import Discord.Interactions
import Discord.Internal.Gateway (CacheHandle(..))
import Discord.Types
import qualified Data.Text.IO as TIO
import qualified Discord.Requests as R

aywins :: IO ()
aywins = do
  runDiscord config >>= TIO.putStrLn

config :: Discord.RunDiscordOpts
config = def { discordToken   = token
             , discordOnEvent = readyHandler <+> eventHandler
             , discordOnEnd   = endHandler
             , discordOnLog   = logHandler
             }

token :: Text
token = "MTAyNjEzMTM5MTMwMzQwMTQ4Mw.GBaOWL.iHQ0ha3GnBrGwfuuQXAXG1ufRrPtOrYI_dph2g"

eventHandler :: Event -> DiscordHandler ()
eventHandler ev = pure ()

readyHandler :: Event -> DiscordHandler ()
readyHandler = \case
  GuildCreate (Guild {guildId}) -> do
    appId <- do
      cacheMVar <- asks $ discordHandleCache .> cacheHandleCache
      cache     <- liftIO $ readMVar cacheMVar >>= \case
        Left (c, _) -> pure c
        Right c     -> pure c
      pure (cacheApplication cache & partialApplicationID)
    response <- restCall $ R.CreateGuildApplicationCommand appId guildId
      CreateApplicationCommandChatInput {
          createName = "testcommand" -- needs to be one word
        , createLocalizedName = Nothing
        , createDescription = "Test Description"
        , createLocalizedDescription = Nothing
        , createOptions = Just . OptionsValues $ [
            OptionValueString {
              optionValueName = "testoption" -- needs to be one word
            , optionValueLocalizedName = Nothing
            , optionValueDescription = "This is a test option"
            , optionValueLocalizedDescription = Nothing
            , optionValueRequired = True
            , optionValueStringChoices = Left False
            , optionValueStringMinLen = Nothing
            , optionValueStringMaxLen = Nothing
            }
          ]
        , createDefaultMemberPermissions = Nothing
        , createDMPermission = Just False
        }
    liftIO $ print response
  _ -> pure ()

endHandler :: IO ()
endHandler = pure ()

logHandler :: Text -> IO ()
logHandler = TIO.putStrLn

-- Default command and option values {{{
defAppCommand :: CreateApplicationCommand
defAppCommand = CreateApplicationCommandChatInput {
    createName = "default" -- needs to be one word
  , createLocalizedName = Nothing
  , createDescription = ""
  , createLocalizedDescription = Nothing
  , createOptions = Just $ OptionsValues []
  , createDefaultMemberPermissions = Nothing
  , createDMPermission = Just False
  }

defOptionValueInteger, defOptionValueString, defOptionValueUser :: OptionValue
defOptionValueInteger = OptionValueInteger {
    optionValueName = "default" -- needs to be one word
  , optionValueLocalizedName = Nothing
  , optionValueDescription = ""
  , optionValueLocalizedDescription = Nothing
  , optionValueRequired = True
  , optionValueIntegerChoices = Left False
  , optionValueIntegerMinVal = Nothing
  , optionValueIntegerMaxVal = Nothing
  }

defOptionValueString = OptionValueString {
    optionValueName = "default" -- needs to be one word
  , optionValueLocalizedName = Nothing
  , optionValueDescription = ""
  , optionValueLocalizedDescription = Nothing
  , optionValueRequired = True
  , optionValueStringChoices = Left False
  , optionValueStringMinLen = Nothing
  , optionValueStringMaxLen = Nothing
  }

defOptionValueUser = OptionValueUser {
    optionValueName = "default" -- needs to be one word
  , optionValueLocalizedName = Nothing
  , optionValueDescription = ""
  , optionValueLocalizedDescription = Nothing
  , optionValueRequired = True
  }
-- }}}

applicationCommands :: [CreateApplicationCommand]
applicationCommands = [
    defAppCommand
  , defAppCommand
  ]
