module InputFile (AppState,
                  pickRandom,
                  readWithReload) where
import Prelude hiding (lines)
import Control.Concurrent (threadDelay)
import qualified Control.Concurrent.Async as A
import Control.Concurrent.MVar (MVar, newMVar, modifyMVar_, withMVar)
import Control.Monad (void)
import qualified Data.ByteString as B
import Data.ByteString.Char8 (lines)
import System.Posix.Files (getFileStatus, modificationTime)
import System.Random (randomRIO)

type InputURL = B.ByteString
type AppState = MVar [InputURL]

seconds :: Int -> Int
seconds n = n * 1000000

pickRandom :: AppState -> IO InputURL
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
        getLines = fmap lines $ B.readFile fn
        getMT    = getFileStatus fn >>= return . modificationTime

loopM :: Monad m => a -> (a -> m a) -> m b
{-# INLINE loopM #-}
loopM inp act = let a' i = act i >>= a' in a' inp
