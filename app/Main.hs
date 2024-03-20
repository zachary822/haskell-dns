{-# LANGUAGE OverloadedStrings #-}

module Main where

import Control.Exception qualified as E
import Data.ByteString.Builder qualified as B
import Network.DNS.Parser
import Network.DNS.Utils
import Network.Socket
import Network.Socket.ByteString (recv)
import Network.Socket.ByteString.Lazy (sendAll)
import System.Random.Stateful

main :: IO ()
main = do
  gen <- initStdGen >>= newAtomicGenM

  -- "198.41.0.4"
  let hints = defaultHints{addrFamily = AF_INET, addrSocketType = Datagram}
  addr : _ <- getAddrInfo (Just hints) (Just "198.41.0.4") (Just "53")

  E.bracket
    ( E.bracketOnError (openSocket addr) close $ \sock -> do
        connect sock (addrAddress addr)
        return sock
    )
    close
    $ \s -> do
      id_ <- uniformWord16 gen
      let q = query id_ "blog2.thoughtbank.app" 1 1

      sendAll s $ B.toLazyByteString q
      msg <- recv s 4096

      let thing = parseDns msg

      print thing
