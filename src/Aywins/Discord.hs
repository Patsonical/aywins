{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE OverloadedStrings #-}

module Aywins.Discord where

import Aywins.ApplicationCommands
import Aywins.CommandParser
import Aywins.Commands
import Aywins.Lib
import Aywins.Responses
import Aywins.Types
import Control.Concurrent.MVar
import Control.Monad (when)
import Control.Monad.IO.Class (MonadIO(..))
import Control.Monad.Trans.Reader (asks)
import Data.Function ((&))
import Data.List (find)
import Data.Text (Text)
import Discord
import Discord.Handle (DiscordHandle(..))
import Discord.Interactions
import Discord.Internal.Gateway (CacheHandle(..))
import Discord.Types
import qualified Data.Text.IO as TIO
import qualified Discord.Requests as R

aywins :: Text -> IO ()
aywins token = do
  guildMVar <- newEmptyMVar
  runDiscord (config token guildMVar) >>= TIO.putStrLn

config :: Text -> MVar Guild -> Discord.RunDiscordOpts
config token guildMVar =
  def { discordToken   = token
      , discordOnEvent = eventHandler guildMVar
      , discordOnEnd   = endHandler
      , discordOnLog   = logHandler
      }

eventHandler :: MVar Guild -> Event -> DiscordHandler ()
eventHandler guildMVar = \case
  GuildCreate guild@(Guild {guildId}) -> do
    success <- liftIO $ tryPutMVar guildMVar guild
    when success $ do
      appId <- do
        cacheMVar <- asks $ discordHandleCache .> cacheHandleCache
        cache     <- liftIO $ readMVar cacheMVar >>= \case
          Left (c, _) -> pure c
          Right c     -> pure c
        pure (cache & cacheApplication & partialApplicationID)
      --forM_ applicationCommands $ \cmd -> do
      --  r <- restCall $ R.CreateGuildApplicationCommand appId guildId cmd
      --  liftIO $ print r
      r <- restCall $ R.BulkOverWriteGuildApplicationCommand appId
                                                             guildId
                                                             applicationCommands
      liftIO $ logToFile "log/aywins_appCommandRegistration.log" r

  InteractionCreate (InteractionApplicationCommand
    { applicationCommandData = ApplicationCommandDataChatInput
        { applicationCommandDataName
        , optionsData
        }
    , interactionId
    , interactionToken
    , interactionUser = MemberOrUser (Left guildMember)
    }) -> do
      let getAdminRole  = find (\r -> roleName r == "AywinsAdmin")
          options       = case optionsData of
            Just (OptionsDataValues o) -> o
            _                          -> []
          command_maybe = parseCommand applicationCommandDataName options
      adminRole_maybe <- getAdminRole . guildRoles <$> liftIO (readMVar guildMVar)
      status <- case (adminRole_maybe, command_maybe) of
        (Nothing       , _           ) -> pure $ Failure AdminRoleMissingError
        (_             , Nothing     ) -> pure $ Failure ParseFailureError
        (Just adminRole, Just command) -> handleCommand guildMember (roleId adminRole) command
      response <- handleStatus status
      liftIO $ logToFile "log/aywins_status.log" response
      r <- restCall $ R.CreateInteractionResponse interactionId interactionToken response
      liftIO $ logToFile "log/aywins_response.log" r

  other -> liftIO $ logToFile "log/aywins_otherEvents.log" other

endHandler :: IO ()
endHandler = pure ()

logHandler :: Text -> IO ()
logHandler = logToFile "log/aywins_logHandler.log"
