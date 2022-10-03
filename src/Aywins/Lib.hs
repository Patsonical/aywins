{-# LANGUAGE RankNTypes #-}

module Aywins.Lib where

(<+>) :: (Applicative f, Semigroup m) => (a -> f m) -> (a -> f m) -> a -> f m
fx <+> fy = \a -> (<>) <$> fx a <*> fy a

(.>) :: (a -> b) -> (b -> c) -> a -> c
(.>) = flip (.)
