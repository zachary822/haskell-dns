{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}

module Network.DNS.Parser where

import Control.Monad.Trans.Class
import Control.Monad.Trans.State.Strict qualified as Trans
import Data.Bifunctor
import Data.Bits
import Data.ByteString qualified as B
import Data.IntMap.Strict qualified as M
import Data.Void
import Data.Word
import Network.DNS.Types
import Text.Megaparsec
import Text.Megaparsec.Byte.Binary

build :: Int -> [B.ByteString] -> [(Int, B.ByteString)]
build _ [] = []
build offset segments@(s : xs) = (offset, B.intercalate "." segments) : build (offset + B.length s + 1) xs

type Parser =
  ParsecT Void B.ByteString (Trans.State (M.IntMap B.ByteString))

pSegment :: Parser B.ByteString
pSegment = word8 >>= takeP (Just "segment") . fromIntegral

pName :: Parser B.ByteString
pName = do
  offset <- getOffset

  (segments, end) <-
    manyTill_ pSegment (satisfy $ \t -> t == 0 || (t .&. 192 /= 0))

  if end == 0
    then do
      -- cache the things
      lift (Trans.modify (M.union (M.fromDistinctAscList (build offset segments))))
      return $ B.intercalate "." segments
    else do
      o <- word8
      let ptr :: Word16 = fromIntegral (end .&. 63) `shiftL` 8 + fromIntegral o
          prefix = B.intercalate "." segments

      suffix <- lift (Trans.gets (M.! fromIntegral ptr))

      if B.null prefix
        then return suffix
        else do
          let c =
                M.fromDistinctAscList (map (second (<> "." <> suffix)) (build offset segments))
          lift (Trans.modify (M.union c))
          return (prefix <> "." <> suffix)

pQuestion :: Parser Question
pQuestion = Question <$> pName <*> word16be <*> word16be

pRR :: Parser RR
pRR = do
  name <- pName
  rtype <- word16be
  rclass <- word16be
  ttl <- word32be
  rdlength <- word16be

  rdata <- case rtype of
    1 -> ARData <$> word32be
    2 -> NSRData <$> pName
    5 -> CNAMERData <$> pName
    6 ->
      SOARData
        <$> pName
        <*> pName
        <*> word32be
        <*> word32be
        <*> word32be
        <*> word32be
        <*> word32be
    12 -> PTRRData <$> pName
    15 -> MXRData <$> word16be <*> pName
    16 -> TXTRData <$> takeP (Just "TXT") (fromIntegral rdlength)
    28 -> AAAARData <$> ((,,,) <$> word32be <*> word32be <*> word32be <*> word32be)
    _ -> RData <$> takeP (Just "RDATA") (fromIntegral rdlength)

  return RR{..}

pDns :: Parser DNS
pDns = do
  id_ <- takeP (Just "id") 2
  flags <- takeP (Just "flags") 2
  qdCount <- word16be
  anCount <- word16be
  nsCount <- word16be
  arCount <- word16be

  qs <- count (fromIntegral qdCount) pQuestion
  ans <- count (fromIntegral anCount) pRR
  nss <- count (fromIntegral nsCount) pRR
  ars <- count (fromIntegral arCount) pRR <* eof

  return DNS{header = DNSHeader{..}, ..}

parseDns :: B.ByteString -> Either String DNS
parseDns msg =
  flip Trans.evalState mempty $
    first errorBundlePretty
      <$> runParserT pDns "" msg
