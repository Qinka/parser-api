{- This file is part of parser-api

   Parser-api is free software: you can redistribute it and/or modify it
   under the terms of the GNU General Public License as published by the
   Free Software Foundation, either version 3 or the License, or (at your option)
   any later version.

   Parser-api is distributed in the hope that it will be useful,
   but WITHOUT ANY WARRANTY; without even the implied warranty of
   MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE. See the GNU General Public
   for more details.

   You should have a received a copy of the GNU General Public License along with parser-api.
   If not, see <http://www.gnu.org/licenses/>
-}

{-|
Module: Text.ParserApi.Parsers
Description: The parsers work with reformat's Reformat
Copyright: (C) 2018 Johann Lee <me@qinka.pro>
License: GPL3
Maintainer: me@qinka.pro
Stability: experimental
Portablility: unknown

This file provides the parsers for parsing the texts.
-}

{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE TypesFamilies    #-}
{-# LANGUAGE  RecordWildCards #-}

module Text.ParserApi.Parsers
  ( IP(..)
  ) where

import Text.Parsec
import Text.Reformat
import Data.Char

-- | parsering the IP
data IP = IP { v4 :: [String] -- ^ ipv4
             , v6 :: [String] -- ^ ipv6
             }
          deriving (Show,Eq)

-- | get the ip
parsingIpx :: Monad m => String -> ParsecT String () m Char -> ParsecT String () m String
parsingIpx i select = do
  spaces
  string i
  skipMany space
  optional $ string "addr"
  spaces
  optional $ string ":"
  spaces
  rs <- many1 select
  skipMany (noneOf "\n\r")
  spaces
  return rs

-- | get the ipv4
parsingIpv4 :: Monad m => ParsecT String () m IP
parsingIpv4 = do
  let select = oneOf "0123456789."
  ip <- parsingIpx "inet"  select
  (\x -> x { v4 = ip : v4 x}) <$> parsingIP
  
parsingIpv6 :: Monad m => ParsecT String () m IP
parsingIpv6 = do
  let select = oneOf "0123456789:aAbBcCdDeEfF"
  ip <- parsingIpx "inet6"  select
  (\x -> x { v6 = ip : v6 x}) <$> parsingIP

parsingIpvNull :: Monad m => ParsecT String () m IP
parsingIpvNull =  skipMany (noneOf "\n\r") >> spaces >> parsingIP


parsingIP :: Monad m => ParsecT String () m IP
parsingIP = try parsingIpEOF <|> try parsingIpv6 <|> try parsingIpv4 <|> parsingIpvNull

parsingIpEOF :: Monad m => ParsecT String () m IP
parsingIpEOF = eof >> return IP {v4 = [], v6 = []}

instance Reformat IP where
  type Str IP = String
  parser = parsingIP
  rednerPair IP{..} var =
    let index = read $ takeWhile isDigit var
        typ   = dropWhile isDigit var
    in case typ of
      "v6" -> S $ v6 !! index
      _    -> S $ v4 !! index
