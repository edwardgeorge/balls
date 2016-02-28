{-# LANGUAGE OverloadedStrings #-}
module Main where
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
  putStrLn $ printf " listening on http://%s:%d/" host port
  mvar <- readWithReload (imageFile opts)
  W.runSettings settings (SAPI.app mvar)
