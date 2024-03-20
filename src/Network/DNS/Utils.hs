module Network.DNS.Utils where

import Data.ByteString qualified as B
import Data.ByteString.Builder qualified as B
import Data.Word

query :: Word16 -> B.ByteString -> Word16 -> Word16 -> B.Builder
query id_ domain qtype qclass =
  B.word16BE id_
    <> qr
    <> ra
    <> qdcount
    <> ancount
    <> nscount
    <> arcount
    <> question domain qtype qclass
 where
  qr = B.word8 0b00000001 -- QR, Opcode, AA, TC, RD
  ra = B.word8 0b00100000 -- RA, Z, RCode
  qdcount = B.word16BE 1
  ancount = B.word16BE 0
  nscount = B.word16BE 0
  arcount = B.word16BE 0

question :: B.ByteString -> Word16 -> Word16 -> B.Builder
question domain qtype qclass =
  formatQname domain
    <> B.word16BE qtype
    <> B.word16BE qclass

formatQname :: B.ByteString -> B.Builder
formatQname q = foldMap f (B.split 46 q) <> B.word8 0
 where
  f segment = B.word8 (fromIntegral $ B.length segment) <> B.byteString segment
