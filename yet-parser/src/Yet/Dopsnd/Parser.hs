{- |
Module      : Yet.Dopsnd.Parser
Description : The parsers
Copyright   : (C) 2018 Johann Lee
License     : GPLv3
Maintainer  : me@qinka.pro
Portability : Unknown

Parsers to get the informations
-}

{-# LANGUAGE FlexibleContexts      #-}
{-# LANGUAGE MultiParamTypeClasses #-}

module Yet.Dopsnd.Parser
  ( parsingIP
  , parsingV4
  , parsingV6
  , parsingEth
  ) where

import qualified Data.ByteString    as B
import qualified Data.Text          as T
import qualified Data.Text.Encoding as T
import           Text.Parsec
import           Yet.Dopsnd.Types

-- | parse ip string with given prefix and chars
parsingIP :: Stream s m Char
          => String -- ^ prefix
          -> ParsecT s u m Char -- ^ chars
          -> ParsecT s u m String
parsingIP pre chars = do
  spaces
  string pre
  spaces
  optional $ string "addr"
  spaces
  optional $ string ":"
  spaces
  cs <- many1 chars
  spaces
  return cs

-- | parse ipv4 address
parsingV4 :: Stream s m Char
          => ParsecT s (Eth String) m ()
parsingV4 =
  parsingIP "inet"   (oneOf "0123456789.")
  >>= \ip -> modifyState (addV4 ip)

-- | parse ipv6 address
parsingV6 :: Stream s m Char
          => ParsecT s (Eth String) m ()
parsingV6 =
  parsingIP "inet6"  (oneOf "0123456789:aAbBcCdDeEfF")
  >>= \ip -> modifyState (addV6 ip)

-- | parse eth infors
parsingEth :: Stream s m Char
           => ParsecT s (Eth String) m (Eth String)
parsingEth = try pEOF <|> try pV4 <|> try pV6 <|> pSkip
  where pEOF  = space >> eof >> getState
        pV4   = parsingV4 >> parsingEth
        pV6   = parsingV6 >> parsingEth
        pSkip = skipMany (noneOf "\r\n") >> parsingEth

