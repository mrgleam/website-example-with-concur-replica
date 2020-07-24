{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Lib
  ( server,
  )
where

import Concur.Core (Widget, liftSTM, orr)
import Concur.Replica (run)
import Control.Concurrent.STM
import qualified Concur.Replica.DOM as H
import qualified Concur.Replica.DOM.Events as P
import Control.Monad.IO.Class (liftIO)
import Data.Text (pack, unpack)
import Data.Text.Encoding (decodeUtf8)
import Network.WebSockets (RequestHead, defaultConnectionOptions, requestPath)
import Network.Wai.Handler.Replica (Context (Context, call, registerCallback))
import Replica.VDOM (HTML, defaultIndex)
import Prelude

class Route a where
  fromRoute :: String -> a
  toRoute :: a -> String

data AppUpdate a b
  = UpdateChangeUrl a
  | UpdateExit b

route :: forall a b. Route a => Context -> a -> (a -> Widget HTML (AppUpdate a b)) -> Widget HTML b
route Context {call, registerCallback} initial f = do
  chan <- liftIO newHistoryChan
  go initial chan
  where
    newHistoryChan :: IO (TChan String)
    newHistoryChan = do
      chan <- newTChanIO
      cb <- registerCallback $ \path -> atomically (writeTChan chan path)
      call cb "window.onpopstate = function(event) { callCallback(arg, location.pathname); };"
      pure chan

    go :: a -> TChan String -> Widget HTML b
    go a chan = do
      r <- orr [Left <$> f a, Right <$> liftSTM (readTChan chan)]
      case r of
        Left (UpdateChangeUrl a') -> do
          liftIO $ call (toRoute a') "window.history.pushState({}, \"\", arg);"
          go a' chan
        Left (UpdateExit b) ->
          pure b
        Right path ->
          go (fromRoute path) chan

--------------------------------------------------------------------------------

data State
  = SiteA
  | SiteB String
  | SiteC Double

instance Route State where
  toRoute = \case
    SiteA -> "/"
    SiteB b -> "/b-" <> b
    SiteC c -> "/c-" <> show c

  fromRoute = \case
    "/" ->
      SiteA
    '/' : 'b' : '-' : b ->
      SiteB b
    '/' : 'c' : '-' : c ->
      SiteC (read c)
    _ ->
      error "Invalid URL"

routingApp :: State -> Widget HTML (AppUpdate State ())
routingApp = \case
  SiteA -> do
    _ <- H.div [P.onClick] [H.text "Site A (click for next site)"]
    pure $ UpdateChangeUrl (SiteB "Next")
  SiteB b -> do
    _ <- H.div [P.onClick] [H.text ("Site B: " <> pack (show b))]
    pure $ UpdateChangeUrl (SiteC 66.6)
  SiteC c -> do
    _ <- H.div [P.onClick] [H.text ("Site C: " <> pack (show c))]
    pure $ UpdateChangeUrl SiteA

server :: IO ()
server = do
  putStrLn "Starting app"
  run 8080 (defaultIndex "Website" []) defaultConnectionOptions id $
    \req ctx -> do
      liftIO (putStrLn "Client connected")
      route ctx (fromRoute (getPath req)) routingApp
  where
    getPath :: RequestHead -> String
    getPath =
      unpack . decodeUtf8 . requestPath
