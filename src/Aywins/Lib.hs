{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE LambdaCase #-}

module Aywins.Lib where
import Data.Time (getCurrentTime)

(<+>) :: (Applicative f, Semigroup m) => (a -> f m) -> (a -> f m) -> a -> f m
fx <+> fy = \a -> (<>) <$> fx a <*> fy a

(.>) :: (a -> b) -> (b -> c) -> a -> c
(.>) = flip (.)

leftToMaybe :: Either a b -> Maybe a
leftToMaybe = \case
  Left x  -> Just x
  Right _ -> Nothing

rightToMaybe :: Either a b -> Maybe b
rightToMaybe = \case
  Left _  -> Nothing
  Right x -> Just x

logToFile :: Show a => FilePath -> a -> IO ()
logToFile file x = do
  time <- getCurrentTime
  let toLog = concat [ show time, ": ",  show x, "\n" ]
  appendFile file toLog
