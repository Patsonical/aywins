{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE NamedFieldPuns #-}

module Aywins.Discord where

import Aywins.ApplicationCommands
import Aywins.Lib
import Aywins.Commands
import Aywins.CommandParser
import Aywins.Responses
import Control.Concurrent.MVar
import Control.Monad.IO.Class (MonadIO(..))
import Control.Monad.Trans.Reader (asks)
import Data.Function ((&))
import Data.Text (Text)
import Data.List (find)
import Discord
import Discord.Handle (DiscordHandle(..))
import Discord.Internal.Gateway (CacheHandle(..))
import Discord.Types
import qualified Data.Text.IO as TIO
import qualified Discord.Requests as R
import Control.Monad (forM_, when)
import Discord.Interactions
import Aywins.Types

aywins :: IO ()
aywins = do
  guildMVar <- newEmptyMVar
  runDiscord (config guildMVar) >>= TIO.putStrLn

config :: MVar Guild -> Discord.RunDiscordOpts
config guildMVar =
  def { discordToken   = token
      , discordOnEvent = readyHandler guildMVar <+> eventHandler guildMVar <+> printHandler
      , discordOnEnd   = endHandler
      , discordOnLog   = logHandler
      }

token :: Text
token = "MTAyNjEzMTM5MTMwMzQwMTQ4Mw.GBaOWL.iHQ0ha3GnBrGwfuuQXAXG1ufRrPtOrYI_dph2g"

printHandler :: Event -> DiscordHandler ()
printHandler = liftIO . print

eventHandler :: MVar Guild -> Event -> DiscordHandler ()
eventHandler guildMVar = \case
  InteractionCreate (InteractionApplicationCommand
    { applicationCommandData = ApplicationCommandDataChatInput
        { applicationCommandDataName
        , optionsData = Just (OptionsDataValues optionsData)
        }
    , interactionId
    , interactionToken
    , interactionUser = MemberOrUser (Left guildMember)
    }) -> do
      let getAdminRole = find (\r -> roleName r == "AywinsAdmin")
          command_maybe = parseCommand applicationCommandDataName optionsData
      adminRole_maybe <- getAdminRole . guildRoles <$> liftIO (readMVar guildMVar)
      status <- case (adminRole_maybe, command_maybe) of
        (Nothing       , _           ) -> pure $ Failure AdminRoleMissingError
        (_             , Nothing     ) -> pure $ Failure ParseFailureError
        (Just adminRole, Just command) -> handleCommand guildMember (roleId adminRole) command
      response <- handleStatus status
      r <- restCall $ R.CreateInteractionResponse interactionId interactionToken response
      liftIO $ print r
  _ -> pure ()

readyHandler :: MVar Guild -> Event -> DiscordHandler ()
readyHandler guildMVar = \case
  GuildCreate guild@(Guild {guildId}) -> do
    success <- liftIO $ tryPutMVar guildMVar guild
    when success $ do
      appId <- do
        cacheMVar <- asks $ discordHandleCache .> cacheHandleCache
        cache     <- liftIO $ readMVar cacheMVar >>= \case
          Left (c, _) -> pure c
          Right c     -> pure c
        pure (cache & cacheApplication & partialApplicationID)
      forM_ applicationCommands $ \cmd -> do
        r <- restCall $ R.CreateGuildApplicationCommand appId guildId cmd
        liftIO $ print r
  _ -> pure ()

endHandler :: IO ()
endHandler = pure ()

logHandler :: Text -> IO ()
logHandler = TIO.putStrLn
