{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}
module ServantInternal (HttpVersion) where
import Network.HTTP.Types (HttpVersion)  -- from http-types
import Network.Wai (httpVersion)
import Servant

-- should be implemented in next version of servant
instance HasServer api => HasServer (HttpVersion :> api) where
  type ServerT (HttpVersion :> api) m = HttpVersion -> ServerT api m
  route _ subserver request response =
    let v = httpVersion request
    in route (Proxy :: Proxy api) (subserver v) request response
