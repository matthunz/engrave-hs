{-# LANGUAGE OverloadedStrings #-}

module Engrave.Server where

import Control.Monad
import Control.Monad.Logger
import Data.Aeson.Types
import Data.Conduit.Network
import qualified Data.Foldable as F
import Data.Maybe
import Network.JSONRPC

data Req = Ping deriving (Show, Eq)

instance FromRequest Req where
  parseParams "ping" = Just . const $ return Ping
  parseParams _ = Nothing

instance ToRequest Req where
  requestIsNotif = const False

instance ToJSON Req where
  toJSON = const emptyArray

data Res = Pong deriving (Show, Eq)

instance FromResponse Res where
  parseResult "ping" = Just $ const $ return Pong
  parseResult _ = Nothing

instance ToJSON Res where
  toJSON Pong = emptyArray

respond :: (MonadLoggerIO m) => Respond Req m Res
respond Ping = return $ Right Pong

serve :: (MonadLoggerIO m) => JSONRPCT m ()
serve = do
  qM <- receiveBatchRequest
  case qM of
    Nothing -> do
      return ()
    Just (SingleRequest q) -> do
      rM <- buildResponse respond q
      F.forM_ rM sendResponse
      serve
    Just (BatchRequest qs) -> do
      rs <- catMaybes <$> forM qs (buildResponse respond)
      sendBatchResponse $ BatchResponse rs
      serve

run :: IO ()
run = runStderrLoggingT $ do
  let ss = serverSettings 3000 "::1"
  jsonrpcTCPServer V2 False ss serve
