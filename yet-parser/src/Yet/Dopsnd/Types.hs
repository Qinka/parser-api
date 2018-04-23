{- |
Module      : Yet.Dopsnd.Types
Description : The types used by parsers
Copyright   : (C) 2018 Johann Lee
License     : GPLv3
Maintainer  : me@qinka.pro
Portability : Unknown

The types for parsers, which includes IPv4, IPv6, and so on.
-}

{-# LANGUAGE RecordWildCards #-}

module Yet.Dopsnd.Types
  ( Eth(..)
  , addV4
  , addV6
  ) where

-- | The infos of a eth net interface.
data Eth a = Eth
  { ethV4 :: [a] -- ^ ipv4 addresses
  , ethV6 :: [a] -- ^ ipv6 addresses
  }
  deriving (Show, Eq)

-- | Add an ipv4 ip
addV4 ::     a -- ^ The ipv4 address to be inserted.
      -> Eth a -- ^ The old status
      -> Eth a -- ^ The new status
addV4 ip Eth{..} = Eth (ip:ethV4) ethV6


-- | Add an ipv6 ip
addV6 ::     a -- ^ The ipv6 address to be inserted.
      -> Eth a -- ^ The old status
      -> Eth a -- ^ The new status
addV6 ip Eth{..} = Eth ethV4 (ip:ethV6)
