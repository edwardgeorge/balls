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
import Options.Applicative
import System.Posix.Files (getFileStatus, modificationTime)
import System.Random (randomRIO)
import Text.Printf (printf)

type InputURL = B.ByteString
type AppState = MVar [InputURL]

seconds :: Int -> Int
seconds n = n * 1000000

data Options = Options { getPort :: Int
                       , getHost :: String
                       , imageFile :: String }
             deriving (Show)

options :: Parser Options
options = Options <$> option auto (long "port"
                                <> short 'p'
                                <> metavar "PORT")
                  <*> option auto (long "host"
                                <> short 'H'
                                <> metavar "IPADDR"
                                <> value "127.0.0.1")
                  <*> argument str (metavar "FILE")

main :: IO ()
main = do
  opts <- execParser $ info (helper <*> options) fullDesc
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

pickRandom :: AppState -> IO InputURL
pickRandom = flip withMVar pick
  where pick vals = randomRIO (0, length vals - 1) >>= return . (vals !!)

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


readWithReload :: String -> IO AppState
readWithReload fn = do
  modtime <- getMT
  mvar <- getLines >>= newMVar
  void . A.async . loopM modtime $ \mt -> do threadDelay (seconds 60)
                                             mt' <- getMT
                                             if mt' > mt then do go mvar
                                                                 putStrLn "reloaded input file"
                                                                 return mt'
                                                         else return mt
  return mvar
  where go       = flip modifyMVar_ $ const getLines
        getLines = fmap lines $ B.readFile fn
        getMT    = getFileStatus fn >>= return . modificationTime

loopM :: Monad m => a -> (a -> m a) -> m b
{-# INLINE loopM #-}
loopM inp act = let a' i = act i >>= a' in a' inp
