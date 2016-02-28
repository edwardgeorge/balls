{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE DeriveFunctor #-}
{-# LANGUAGE PolyKinds #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE FlexibleContexts #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}
module ServantInternal (HttpVersion, Redirect) where
import qualified Blaze.ByteString.Builder as BB  -- from blaze-builder
import Control.Monad.Trans.Either (runEitherT)   -- from either
import GHC.TypeLits
import Network.HTTP.Types (HttpVersion,
                           http11,
                           methodGet,
                           status302,
                           status303)            -- from http-types
import Network.Wai (httpVersion,
                    requestMethod,
                    responseLBS)                 -- from wai
import Servant hiding (URI)                      -- from servant-server
import Servant.Server.Internal                   -- from servant-server
import Servant.Server.Internal.ServantErr (
  responseServantErr)                            -- from servant-server
import URI.ByteString (URI, serializeURI)        -- from uri-bytestring

-- should be implemented in next version of servant
instance HasServer api => HasServer (HttpVersion :> api) where
  type ServerT (HttpVersion :> api) m = HttpVersion -> ServerT api m
  route _ subserver request response =
    let v = httpVersion request
    in route (Proxy :: Proxy api) (subserver v) request response

data Redirect (a :: *)

instance HasServer (Redirect URI) where
  type ServerT (Redirect URI) m = m URI
  route Proxy action request respond
    | pathIsEmpty request && requestMethod request == methodGet = do
        e <- runEitherT action
        respond $ case e of
          Left err -> succeedWith $ responseServantErr err
          Right output -> do
            let v = if httpVersion request == http11
                    then status303
                    else status302
                uri = serializeURI output
            succeedWith $ responseLBS v [("Location", BB.toByteString uri)] (BB.toLazyByteString uri)
    | pathIsEmpty request && requestMethod request /= methodGet =
        respond $ failWith WrongMethod
    | otherwise = respond $ failWith NotFound

instance (GetHeaders (Headers h URI), Member h "Location" ~ 'False)
         => HasServer (Redirect (Headers h URI)) where
  type ServerT (Redirect (Headers h URI)) m = m (Headers h URI)
  route Proxy action request respond
    | pathIsEmpty request && requestMethod request == methodGet = do
        e <- runEitherT action
        respond $ case e of
          Left err -> succeedWith $ responseServantErr err
          Right output -> do
            let v = if httpVersion request == http11
                    then status303
                    else status302
                uri = serializeURI $ getResponse output
                headers = getHeaders output
            succeedWith $ responseLBS v (("Location", BB.toByteString uri) : headers) (BB.toLazyByteString uri)
    | pathIsEmpty request && requestMethod request /= methodGet =
        respond $ failWith WrongMethod
    | otherwise = respond $ failWith NotFound

instance (GetHeaders (Headers h ()), Member h "Location" ~ 'True)
         => HasServer (Redirect (Headers h ())) where
  type ServerT (Redirect (Headers h ())) m = m (Headers h ())
  route Proxy action request respond
    | pathIsEmpty request && requestMethod request == methodGet = do
        e <- runEitherT action
        respond $ case e of
          Left err -> succeedWith $ responseServantErr err
          Right output -> do
            let v = if httpVersion request == http11
                    then status303
                    else status302
                headers = getHeaders output
            succeedWith $ responseLBS v headers ""
    | pathIsEmpty request && requestMethod request /= methodGet =
        respond $ failWith WrongMethod
    | otherwise = respond $ failWith NotFound

type family Member (a :: [k]) (b :: Symbol) :: Bool where
  Member '[]                x = 'False
  Member (Header x y ': xs) x = 'True
  Member (x ': xs)          y = Member xs y
