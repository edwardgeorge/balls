module InputFile (AppState,
                  URI,
                  pickRandom,
                  readWithReload) where
import Prelude hiding (lines)
import Control.Concurrent (threadDelay)
import qualified Control.Concurrent.Async as A                -- from async
import Control.Concurrent.MVar (MVar, newMVar, modifyMVar_, withMVar)
import Control.Monad (void)
import qualified Data.ByteString as B                         -- from bytestring
import Data.ByteString.Char8 (lines)                          -- from bytestring
import URI.ByteString (URI, parseURI, strictURIParserOptions) -- from uri-bytestring
import System.Posix.Files (getFileStatus, modificationTime)   -- from unix
import Data.Random (sample, randomElement)                    -- from random-fu

type AppState = MVar [URI]

seconds :: Int -> Int
seconds n = n * 1000000

pickRandom :: AppState -> IO URI
pickRandom = flip withMVar $ sample . randomElement

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
        getLines = do d <- B.readFile fn
                      l <- rightsLog . map (parseURI strictURIParserOptions) $ lines d
                      case l of
                        [] -> error "empty url list provided"
                        _  -> return l
        getMT    = getFileStatus fn >>= return . modificationTime

rightsLog :: Show a => [Either a b] -> IO [b]
rightsLog = foldr go (return [])
  where go (Left  v) b = print v >> b
        go (Right a) b = (a:) <$> b

loopM :: Monad m => a -> (a -> m a) -> m b
{-# INLINE loopM #-}
loopM inp act = let a' i = act i >>= a' in a' inp
