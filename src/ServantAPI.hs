{-# LANGUAGE DataKinds #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RankNTypes #-}
module ServantAPI where
import qualified Blaze.ByteString.Builder as BB  -- from blaze-builder
import Control.Monad.IO.Class (liftIO)           -- from transformers
import Control.Monad.Trans.Either (left)         -- from either
import Data.Proxy (Proxy(..))
import GHC.TypeLits
import Network.HTTP.Types (http11)  -- from http-types
import qualified Network.HTTP.Types.Header as HT -- from http-types
import Network.Wai (Application)                 -- from wai
import Servant ((:>),
                (:<|>)(..),
                serve,
                AddHeader,
                addHeader,
                Header,
                Headers,
                HasServer,
                Get,
                Server,
                ServantErr(..),
                err302,
                err303)                         -- from servant-server
import URI.ByteString (serializeURI)            -- from uri-bytestring

import InputFile
import ServantInternal

type CacheHeaders = '[Header "Cache-Control" String,
                      Header "Pragma" String,
                      Header "Expire" String]

type API n = (n :: Symbol) :> Redirect (Headers  CacheHeaders URI)

cacheHeaders :: (AddHeader "Cache-Control" String orig c,
                 AddHeader "Pragma"        String orig1 orig,
                 AddHeader "Expire"        String orig2 orig1)
             => orig2 -> c
cacheHeaders = addHeader "no-cache, no-store, must-revalidate"
             . addHeader "no-cache"
             . addHeader "0"

server :: proxy (n :: Symbol) -> AppState -> Server (API n)
server _ st = do url <- liftIO $ pickRandom st
                 return $ cacheHeaders url

app :: [(String, AppState)] -> Application
app as = makeServer as server

makeServer :: [(String, a)] -> (forall p n. p (n :: Symbol) -> a -> Server (API n))
           -> Application
makeServer []          _    = error "empty endpoints"
makeServer ((x, a):xs) view = case someSymbolVal x of
  SomeSymbol y -> foo' (bar y) (view y a) xs view serve
  where bar :: Proxy (n :: Symbol) -> Proxy (API n)
        bar _ = Proxy

foo' :: HasServer n => Proxy n -> Server n -> [(String, a)]
     -> (forall p m. p (m :: Symbol) -> a -> Server (API m))
     -> (forall m. HasServer m => Proxy m -> Server m -> r) -> r
foo' p s []          _    f = f p s
foo' p s ((x, a):xs) view f = case someSymbolVal x of
  SomeSymbol y -> foo' (bar' y p) (view y a :<|> s) xs view f
  where bar' :: Proxy (n :: Symbol) -> Proxy m -> Proxy (API n :<|> m)
        bar' _ _ = Proxy
