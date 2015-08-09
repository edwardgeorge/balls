{-# LANGUAGE OverloadedStrings #-}
module Main where
import Prelude hiding (lines)
import Control.Concurrent (threadDelay)
import qualified Control.Concurrent.Async as A
import Control.Concurrent.MVar (MVar, newMVar, modifyMVar_, withMVar)
import Control.Monad (forever, void)
import qualified Data.ByteString as B
import Data.ByteString.Lazy (fromStrict)
import Data.ByteString.Char8 (lines)
import Data.String (fromString)
import Data.Function ((&))
import Network.Wai
import Network.HTTP.Types
import qualified Network.Wai.Handler.Warp as W
import System.Posix.Files (getFileStatus, modificationTime)
import System.Random (randomRIO)
import Text.Printf (printf)

import CmdOptions
import InputFile

main :: IO ()
main = do
  opts <- parseOptions
  let (host, port) = (getHost opts, getPort opts)
      settings     = W.defaultSettings & W.setPort port
                                       & W.setHost (fromString host)
  putStrLn $ printf " listening on http://%s:%d/" host port
  mvar <- readWithReload (imageFile opts)
  W.runSettings settings (app mvar)

-- strip empty path parts from right of list
stripEmptyR :: (Monoid a, Eq a) => [a] -> [a]
stripEmptyR = foldr go []
  where go x [] = if x == mempty then [] else [x]
        go x ys = x : ys

randomBalls :: AppState -> Request -> IO Response
randomBalls mvar req = do
  url <- pickRandom mvar
  return $ responseLBS (if httpVersion req >= http11 then status303 else status302)
                       [("Content-Type", "text/plain"),
                        ("Location", url)]
                       (fromStrict url)

notFound :: Response
notFound = responseLBS status404
                       [("Content-Type", "text/plain")]
                       "404 Not Found"

app :: AppState -> Application
app mvar req respond = do
  resp <- case stripEmptyR (pathInfo req) of
    "balls":[] -> randomBalls mvar req
    _          -> return notFound
  respond resp
