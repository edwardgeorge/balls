{-# LANGUAGE DataKinds #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RankNTypes #-}
module ServantAPI where
import Control.Monad.IO.Class (liftIO)   -- from transformers
import Data.List.NonEmpty (NonEmpty(..)) -- from semigroups
import Data.Proxy (Proxy(..))
import GHC.TypeLits
import Network.Wai (Application)         -- from wai
import Servant ((:>),
                (:<|>)(..),
                serve,
                AddHeader,
                addHeader,
                Header,
                Headers,
                HasServer,
                Server)                  -- from servant-server

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

app :: NonEmpty (String, AppState) -> Application
app as = makeServer as server

makeServer :: NonEmpty (String, a)
           -> (forall p n. p (n :: Symbol) -> a -> Server (API n))
           -> Application
makeServer ((x, a) :| xs) view = case someSymbolVal x of
  SomeSymbol y -> addAdditional (toAPI y) (view y a) xs view serve
  where toAPI :: Proxy (n :: Symbol) -> Proxy (API n)
        toAPI _ = Proxy

addAdditional :: HasServer n => Proxy n -> Server n -> [(String, a)]
              -> (forall p m. p (m :: Symbol) -> a -> Server (API m))
              -> (forall m. HasServer m => Proxy m -> Server m -> r) -> r
addAdditional p s []          _    f = f p s
addAdditional p s ((x, a):xs) view f = case someSymbolVal x of
  SomeSymbol y -> addAdditional (tlCons y p) (view y a :<|> s) xs view f
  where tlCons :: Proxy (n :: Symbol) -> Proxy m -> Proxy (API n :<|> m)
        tlCons _ _ = Proxy
