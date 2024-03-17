{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}

module Network.DNS.Parser where

import Control.Monad.Trans.Class
import Control.Monad.Trans.Except
import Control.Monad.Trans.State.Strict
import Data.Bifunctor
import Data.Bits
import Data.ByteString qualified as B
import Data.IntMap.Strict qualified as M
import Data.Void
import Debug.Trace
import Network.DNS.Types
import Text.Megaparsec
import Text.Megaparsec.Byte
import Text.Megaparsec.Byte.Binary

type Parser = Parsec Void B.ByteString

pSegment :: Parser B.ByteString
pSegment = word8 >>= takeP (Just "segment") . fromIntegral

pName :: Parser Name
pName = do
  (segments, end) <-
    first (B.intercalate ".")
      <$> manyTill_ pSegment (satisfy $ \t -> t == 0 || (t .&. 192 /= 0))

  if end == 0
    then
      return $ FullName segments
    else do
      o <- word8
      return $
        PointerName segments (fromIntegral (end .&. 63) `shiftL` 8 + fromIntegral o)

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

-- pDnsResponse :: Parser DNS
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

  return (DNSHeader{..}, qs, ans, nss, ars)

parseDns msg = do
  thing <-
    runResolvePointer
      msg
      ( do
          (_, _, _, _, ars) <- lift $ except $ runParser pDns "" msg

          traverse resolvePointer (map name ars)
      )

  traceShowM thing

  return thing

type PointerStateT m = StateT (B.ByteString, (M.IntMap B.ByteString)) m

resolvePointer ::
  Name ->
  PointerStateT
    (Except (ParseErrorBundle B.ByteString Void))
    B.ByteString
resolvePointer (FullName n) = return n
resolvePointer (PointerName prefix offset) = do
  msuffix <- gets (M.lookup (fromIntegral offset) . snd)

  resolved <- case msuffix of
    Just s -> return s
    Nothing -> do
      m <- gets (B.drop (fromIntegral offset) . fst)
      name <- lift $ except (parse pName "" m)
      res <- case name of
        (FullName n) -> return n
        nm@(PointerName _ _) -> resolvePointer nm
      modify $ second (M.insert (fromIntegral offset) res)
      return res

  return $ if B.null prefix then resolved else prefix <> "." <> resolved

runResolvePointer ::
  B.ByteString ->
  PointerStateT (Except (ParseErrorBundle B.ByteString Void)) a1 ->
  (Either (ParseErrorBundle B.ByteString Void) a1)
runResolvePointer msg a = runExcept $ evalStateT a (msg, mempty)
