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
          u' <- getState
          return (a,u')

spec :: Spec
spec = do
  describe "Function: parsingIP" $ do
    it "Case 0: normal one" $ do
      runParserTest (parsingIP "prefix" (oneOf "01.")) () "prefixaddr:101.10101.10101.10101d"
        `shouldReturn` ("101.10101.10101.10101",())

      runParserTest (parsingIP "prefix" (oneOf "01.")) () "prefix addr : 101.10101.10101.10101 d"
        `shouldReturn` ("101.10101.10101.10101",())

    it "Case 1: with or withour space" $ do
      runParserTest (parsingIP "prefix" (oneOf "01.")) () " prefixaddr:101.10101.10101.10101d"
        `shouldReturn` ("101.10101.10101.10101",())

      runParserTest (parsingIP "prefix" (oneOf "01.")) () "prefix addr:101.10101.10101.10101d"
        `shouldReturn` ("101.10101.10101.10101",())

      runParserTest (parsingIP "prefix" (oneOf "01.")) () "prefixaddr :101.10101.10101.10101d"
        `shouldReturn` ("101.10101.10101.10101",())

      runParserTest (parsingIP "prefix" (oneOf "01.")) () "prefixaddr: 101.10101.10101.10101d"
        `shouldReturn` ("101.10101.10101.10101",())

      runParserTest (parsingIP "prefix" (oneOf "01.")) () "prefixaddr:101.10101.10101.10101 d"
        `shouldReturn` ("101.10101.10101.10101",())

    it "Case 2: with or without tab(\\t)" $ do
      runParserTest (parsingIP "prefix" (oneOf "01.")) () "\tprefixaddr:101.10101.10101.10101d"
        `shouldReturn` ("101.10101.10101.10101",())

      runParserTest (parsingIP "prefix" (oneOf "01.")) () "prefix\taddr:101.10101.10101.10101d"
        `shouldReturn` ("101.10101.10101.10101",())

      runParserTest (parsingIP "prefix" (oneOf "01.")) () "prefixaddr\t:101.10101.10101.10101d"
        `shouldReturn` ("101.10101.10101.10101",())

      runParserTest (parsingIP "prefix" (oneOf "01.")) () "prefixaddr:\t101.10101.10101.10101d"
        `shouldReturn` ("101.10101.10101.10101",())

      runParserTest (parsingIP "prefix" (oneOf "01.")) () "prefixaddr:101.10101.10101.10101\td"
        `shouldReturn` ("101.10101.10101.10101",())

    it "Case 3: with or without new lines(\\n)" $ do
      runParserTest (parsingIP "prefix" (oneOf "01.")) () "\nprefixaddr:101.10101.10101.10101d"
        `shouldReturn` ("101.10101.10101.10101",())

      runParserTest (parsingIP "prefix" (oneOf "01.")) () "prefix\naddr:101.10101.10101.10101d"
        `shouldReturn` ("101.10101.10101.10101",())

      runParserTest (parsingIP "prefix" (oneOf "01.")) () "prefixaddr\n:101.10101.10101.10101d"
        `shouldReturn` ("101.10101.10101.10101",())

      runParserTest (parsingIP "prefix" (oneOf "01.")) () "prefixaddr:\n101.10101.10101.10101d"
        `shouldReturn` ("101.10101.10101.10101",())

      runParserTest (parsingIP "prefix" (oneOf "01.")) () "prefixaddr:101.10101.10101.10101\nd"
        `shouldReturn` ("101.10101.10101.10101",())

  describe "Function parsingV4" $ do
    it "Case 0: Ubuntu(Linux)" $ do
      runParserTest parsingV4 (Eth [] [])
        "inet 172.18.0.1  netmask 255.255.0.0  broadcast 172.18.255.255"
        `shouldReturn` ((), Eth ["172.18.0.1"] [])
    it "Case 1: OpenWRT(Linux)" $ do
      runParserTest parsingV4 (Eth [] [])
        "inet addr:10.170.68.91  P-t-P:10.170.72.254  Mask:255.255.255.255"
        `shouldReturn` ((), Eth ["10.170.68.91"] [])
    it "Case 2: Debian(Linux)" $ do
      runParserTest parsingV4 (Eth [] [])
        " inet addr:155.94.190.255  P-t-P:155.94.190.24  Bcast:155.94.190.24  Mask:255.255.255.255"
        `shouldReturn` ((), Eth ["155.94.190.255"] [])
    it "Case 3: LEDE(Linux)" $ do
      runParserTest parsingV4 (Eth [] [])
        "\n\t inet addr:127.0.0.1  Mask:255.0.0.0\n"
        `shouldReturn` ((), Eth ["127.0.0.1"] [])
    it "Case 4: macOS(Darwin)" $ do
      runParserTest parsingV4 (Eth [] [])
        "inet 127.0.0.1 netmask 0xff000000"
        `shouldReturn` ((), Eth ["127.0.0.1"] [])

  describe "Function parsingV6" $ do
    it "Case 0: Ubuntu(Linux)" $ do
      runParserTest parsingV6 (Eth [] [])
        "inet6 fe80::9409:bdff:fedc:7e8d  prefixlen 64  scopeid 0x20<link>"
        `shouldReturn` ((), Eth [] ["fe80::9409:bdff:fedc:7e8d"])
    it "Case 1: OpenWRT(Linux)" $ do
      runParserTest parsingV6 (Eth [] [])
        " inet6 addr: 2002:250:1006:dff0:5d7d:d266:cb7f:1a16/64 Scope:Global"
        `shouldReturn` ((), Eth [] ["2002:250:1006:dff0:5d7d:d266:cb7f:1a16"])
    it "Case 2: Debian(Linux)" $ do
      runParserTest parsingV6 (Eth [] [])
        " inet6 addr: 3607:fcd0:100:6e05::7cf7:5855/128 Scope:Global"
        `shouldReturn` ((), Eth [] ["3607:fcd0:100:6e05::7cf7:5855"])
    it "Case 3: LEDE(Linux)" $ do
      runParserTest parsingV6 (Eth [] [])
        "\n\t   inet6 addr: fe80::40d6:f0ff:fe8e:ea4e/64 Scope:Link\n"
        `shouldReturn` ((), Eth [] ["fe80::40d6:f0ff:fe8e:ea4e"])
    it "Case 4: macOS(Darwin)" $ do
      runParserTest parsingV6 (Eth [] [])
        " inet6 fe80::b396:3e82:7404:b17d%utun0 prefixlen 64 scopeid 0xc"
        `shouldReturn` ((), Eth [] ["fe80::b396:3e82:7404:b17d"])

  describe "Function parsingEth" $ do
    it "Case 0: Debian(Linux)" $ do
      let eth = Eth ["10.0.0.1"] ["fe80::689c:9bff:feed:1865"]
      runParserTest parsingEth (Eth [] [])
        "lkl-tap   Link encap:Ethernet  HWaddr \n         inet addr:10.0.0.1  Bcast:0.0.0.0  Mask:255.255.255.0\n           inet6 addr: fe80::689c:9bff:feed:1865/64 Scope:Link\n          UP BROADCAST RUNNING MULTICAST  MTU:1500  Metric:1\n          RX packets:1136125 errors:0 dropped:0 overruns:0 frame:0\n          TX packets:1371303 errors:0 dropped:1 overruns:0 carrier:0\n          collisions:0 txqueuelen:500\n          RX bytes:496472944 (473.4 MiB)  TX bytes:528418824 (503.9 MiB)"
        `shouldReturn` (eth,eth)
