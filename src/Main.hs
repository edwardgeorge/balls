{-# LANGUAGE OverloadedStrings #-}
module Main where
import Control.Monad (void)
import Data.List.NonEmpty (NonEmpty(..))       -- from semigroups
import Data.String (fromString)
import Data.Function ((&))
import qualified Network.Wai.Handler.Warp as W -- from warp
import Text.Printf (printf)

import CmdOptions
import InputFile

import qualified ServantAPI as SAPI

main :: IO ()
main = do
  opts <- parseOptions
  let (host, port) = (getHost opts, getPort opts)
      settings     = W.defaultSettings & W.setPort port
                                       & W.setHost (fromString host)
      files        = ("balls", imageFile opts) :| extraImages opts
  routes <- sequence $ fmap (sequence . fmap readWithReload) files
  putStrLn $ printf " listening on http://%s:%d/" host port
  void . sequence $ fmap (\(a, b) -> putStrLn $ " - /" ++ a ++ " from: " ++ b) files
  W.runSettings settings $ SAPI.app routes
