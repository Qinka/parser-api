{-# LANGUAGE FlexibleContexts #-}

module Yet.DopsndSpec
  ( spec
  ) where


import Text.Parsec
import Yet.Dopsnd
import Yet.Dopsnd.Types
import Yet.Dopsnd.Parser
import           Test.Hspec
import Data.Functor.Identity



runParserTest :: Stream s Identity Char
              => Parsec s u a -> u -> s -> IO (a,u)
runParserTest p u s =
  case runParser p' u "" s of
    Left err -> error $ show err
    Right x  -> return x
  where p' = do
          a <- p
          u <- getState
          return (a,u)


spec :: Spec
spec = do
  describe "parsingIP" $ do
    it "ipv4" $ do
      runParserTest (parsingIP "prefix" (oneOf "01.")) () "  prefix addr : 101.10101.10101.10101d"
        `shouldReturn` ("101.10101.10101.10101",())
