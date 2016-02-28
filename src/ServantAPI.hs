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
                HasServer,
                Get,
                Server,
                ServantErr(..),
                err302,
                err303)                         -- from servant-server
import URI.ByteString (serializeURI)            -- from uri-bytestring

import InputFile
import ServantInternal

type API n = (n :: Symbol) :> HttpVersion :> Get '[] ()

cacheHeaders' :: [HT.Header]
cacheHeaders' = [(HT.hCacheControl, "no-cache, no-store, must-revalidate"),
                 (HT.hPragma,       "no-cache"),
                 (HT.hExpires,      "0")]

-- can only do this with an error currently
redirect :: HttpVersion -> URI -> ServantErr
redirect v u = let uri = serializeURI u
                   res = if v == http11 then err303 else err302
               in res {
  errBody    = BB.toLazyByteString uri,
  errHeaders = (HT.hLocation, BB.toByteString uri) :
               (HT.hContentType, "text/plain") : cacheHeaders' }

server :: proxy (n :: Symbol) -> AppState -> Server (API n)
server _ st v = do url <- liftIO $ pickRandom st
                   left $ redirect v url

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
