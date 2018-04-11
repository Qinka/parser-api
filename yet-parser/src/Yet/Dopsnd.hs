{- |
Module      : Yet.Dopsnd
Description : The parsers
Copyright   : (C) 2018 Johann Lee
License     : GPLv3
Maintainer  : me@qinka.pro
Portability : Unknown

Parsing inet ips.
-}

{-# LANGUAGE FlexibleContexts      #-}
{-# LANGUAGE MultiParamTypeClasses #-}

module Yet.Dopsnd
  (
  ) where

import           Text.Parsec
import           Yet.Dopsnd.Parser
import           Yet.Dopsnd.Types

-- parsing eth info from string
parseEth ::Stream s Identity t
         => SourceName
         -> s  -- ^ strings
         -> Either String (Eth String)
parseEth sn s =
  let u = Eth [] []
  in case runParser parsingEth u sn s of
    Left e    -> Left $ show e
    Right eth -> Right eth

