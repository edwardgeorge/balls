{-# LANGUAGE DataKinds #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE OverloadedStrings #-}
module ServantAPI where
import qualified Blaze.ByteString.Builder as BB  -- from blaze-builder
import Control.Monad.IO.Class (liftIO)           -- from transformers
import Control.Monad.Trans.Either (left)         -- from either
import Data.Proxy (Proxy(..))
import qualified Network.HTTP.Types.Header as HT -- from http-types
import Network.Wai (Application)                 -- from wai
import Servant ((:>),
                AddHeader,
                addHeader,
                Get,
                serve,
                Server,
                ServantErr(..),
                err302)                         -- from servant
import URI.ByteString (serializeURI)            -- from uri-bytestring



import InputFile

-- type API = "balls" :> Get '[PlainText] (Headers
--                                         '[Header "Cache-Control" String,
--                                           Header "Pragma"        String,
--                                           Header "Expire"        String,
--                                           Header "Location"      ByteString]
--                                         String)

type API = "balls" :> Get '[] ()

cacheHeaders :: (AddHeader "Cache-Control" String orig c,
                 AddHeader "Pragma"        String orig1 orig,
                 AddHeader "Expire"        String orig2 orig1)
             => orig2 -> c
cacheHeaders = addHeader "no-cache, no-store, must-revalidate"
             . addHeader "no-cache"
             . addHeader "0"

cacheHeaders' :: [HT.Header]
cacheHeaders' = [(HT.hCacheControl, "no-cache, no-store, must-revalidate"),
                 (HT.hPragma,       "no-cache"),
                 (HT.hExpires,      "0")]

-- can only do this with an error currently
redirect :: URI -> ServantErr
redirect u = let uri = serializeURI u
             in err302 { -- TODO: 303 if HTTP1.1, when HttpVersion lands in servant
  errBody    = BB.toLazyByteString uri,
  errHeaders = (HT.hLocation, BB.toByteString uri) :
               (HT.hContentType, "text/plain") : cacheHeaders' }

server :: AppState -> Server API
server st = do url <- liftIO $ pickRandom st
               left $ redirect url

app :: AppState -> Application
app st = serve (Proxy :: Proxy API) $ server st
