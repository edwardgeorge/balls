module InputFile (AppState,
                  URI,
                  pickRandom,
                  readWithReload) where
import Prelude hiding (lines)
import Control.Concurrent (threadDelay)
import qualified Control.Concurrent.Async as A
import Control.Concurrent.MVar (MVar, newMVar, modifyMVar_, withMVar)
import Control.Monad (void)
import qualified Data.ByteString as B
import Data.ByteString.Char8 (lines)
import URI.ByteString (URI, parseURI, strictURIParserOptions) -- from uri-bytestring
import System.Posix.Files (getFileStatus, modificationTime)
import System.Random (randomRIO)

type AppState = MVar [URI]

seconds :: Int -> Int
seconds n = n * 1000000

pickRandom :: AppState -> IO URI
pickRandom = flip withMVar pick
  where pick vals = randomRIO (0, length vals - 1) >>= return . (vals !!)

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
                      rightsLog . map (parseURI strictURIParserOptions) $ lines d
        getMT    = getFileStatus fn >>= return . modificationTime

rightsLog :: Show a => [Either a b] -> IO [b]
rightsLog = foldr go (return [])
  where go (Left  v) b = print v >> b
        go (Right a) b = (a:) <$> b

loopM :: Monad m => a -> (a -> m a) -> m b
{-# INLINE loopM #-}
loopM inp act = let a' i = act i >>= a' in a' inp
