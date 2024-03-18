module Network.DNS.Types where

import Data.ByteString (ByteString)
import Data.Word

data DNSHeader = DNSHeader
  { id_ :: !ByteString
  , flags :: !ByteString
  , qdCount :: !Word16
  , anCount :: !Word16
  , nsCount :: !Word16
  , arCount :: !Word16
  }
  deriving (Show, Eq)

data DNS = DNS
  { header :: !DNSHeader
  , qs :: ![Question]
  , ans :: ![RR]
  , nss :: ![RR]
  , ars :: ![RR]
  }
  deriving (Show, Eq)

data Question = Question
  { qname :: !ByteString
  , qtype :: !Word16
  , qclass :: !Word16
  }
  deriving (Show, Eq)

data RR = RR
  { name :: !ByteString
  , rtype :: !Word16
  , rclass :: !Word16
  , ttl :: !Word32
  , rdlength :: !Word16
  , rdata :: !RData
  }
  deriving (Show, Eq)

data RData
  = RData !ByteString
  | ARData !Word32
  | NSRData !ByteString
  | CNAMERData !ByteString
  | SOARData
      { mname :: !ByteString
      , rname :: !ByteString
      , serial :: !Word32
      , refresh :: !Word32
      , retry :: !Word32
      , expire :: !Word32
      , minimum :: !Word32
      }
  | PTRRData !ByteString
  | MXRData !Word16 !ByteString
  | TXTRData !ByteString
  | AAAARData !(Word32, Word32, Word32, Word32)
  deriving (Show, Eq)
