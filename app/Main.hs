{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}

module Main where

import Control.Exception qualified as E
import Crypto.Random
import Data.ByteString qualified as B
import Data.ByteString.Builder qualified as B
import Network.DNS.Parser
import Network.Socket
import Network.Socket.ByteString (recv)
import Network.Socket.ByteString.Lazy (sendAll)

main :: IO ()
main = do
  -- "198.41.0.4"
  let hints = defaultHints{addrFamily = AF_INET, addrSocketType = Datagram}
  addr <- head <$> getAddrInfo (Just hints) (Just "1.1.1.1") (Just "53")

  E.bracket
    ( E.bracketOnError (openSocket addr) close $ \sock -> do
        connect sock (addrAddress addr)
        return sock
    )
    close
    $ \s -> do
      id_ <- getRandomBytes 2
      let qr = B.word8 0b00000001 -- QR, Opcode, AA, TC, RD
          ra = B.word8 0b00100000 -- RA, Z, RCode
          qdcount = B.word16BE 1
          ancount = B.word16BE 0
          nscount = B.word16BE 0
          arcount = B.word16BE 0
      let qname =
            formatQname "blog2.thoughtbank.app"
              <> B.word16BE 1
              <> B.word16BE 1

      let packet =
            mconcat
              [B.byteString id_, qr, ra, qdcount, ancount, nscount, arcount, qname]

      sendAll s $ B.toLazyByteString packet
      msg <- recv s 4096

      let thing = parseDns msg

      print thing

formatQname :: B.ByteString -> B.Builder
formatQname q = foldMap f (B.split 46 q) <> B.word8 0
 where
  f segment = B.word8 (fromIntegral $ B.length segment) <> B.byteString segment
