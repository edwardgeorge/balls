{-# LANGUAGE OverloadedStrings #-}
module Main where
import qualified Data.ByteString as BS
import Data.ByteString.Lazy (fromStrict)
import Data.String (fromString)
import Data.Function ((&))
import Network.Wai
import Network.HTTP.Types
import qualified Network.Wai.Handler.Warp as W
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
                        ("Location", url),
                        ("Cache-Control", "no-cache, no-store, must-revalidate"),
                        ("Pragma", "no-cache"),
                        ("Expire", "0")]
                       (fromStrict url)

notFound :: Response
notFound = responseLBS status404
                       [("Content-Type", "text/plain")]
                       "404 Not Found"

onlyMethods :: [Method] -> Request -> (Request -> IO Response) -> IO Response
onlyMethods methods req app = do
  if requestMethod req `elem` methods
  then app req
  else return $ responseLBS status405
                            [("Allow", BS.intercalate ", " methods)]
                            "Method Not Allowed"

app :: AppState -> Application
app mvar req respond = do
  resp <- case stripEmptyR (pathInfo req) of
    "balls":[] -> onlyMethods ["GET"] req $ randomBalls mvar
    _          -> return notFound
  respond resp
