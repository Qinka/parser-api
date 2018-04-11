{- |
Module      : Yet.Dopsnd.Types
Description : The types used by parsers
Copyright   : (C) 2018 Johann Lee
License     : GPLv3
Maintainer  : me@qinka.pro
Portability : Unknown

The types for parsers, which includes IPv4, and so on.
-}

{-# LANGUAGE RecordWildCards #-}

module Yet.Dopsnd.Types
  ( Eth(..)
  , addV4
  , addV6
  ) where

-- | ethernet informations
data Eth a = Eth
  { ethV4 :: [a] -- ^ ipv4 addresses
  , ethV6 :: [a] -- ^ ipv6 addresses
  }
  deriving (Show, Eq)

-- add an ipv4 ip
addV4 :: a -> Eth a -> Eth a
addV4 Eth{..} ip = Eth (ip:ethV4) ethV6


-- add an ipv6 ip
addV6 :: a -> Eth a -> Eth a
addV6 Eth{..} ip = Eth ethV4 (ip:ethV6)
