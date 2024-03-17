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

data Question = Question
  { qname :: !Name
  , qtype :: !Word16
  , qclass :: !Word16
  }
  deriving (Show, Eq)

data RR = RR
  { name :: !Name
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
  | NSRData !Name
  | CNAMERData !Name
  | SOARData
      { mname :: !Name
      , rname :: !Name
      , serial :: !Word32
      , refresh :: !Word32
      , retry :: !Word32
      , expire :: !Word32
      , minimum :: !Word32
      }
  | PTRRData !Name
  | MXRData !Word16 !Name
  | TXTRData !ByteString
  | AAAARData !(Word32, Word32, Word32, Word32)
  deriving (Show, Eq)

data Name
  = FullName !ByteString
  | PointerName !ByteString !Word16
  deriving (Show, Eq)
