import Control.Concurrent
import Control.Exception (IOException)
import Control.Monad.State
import System.Environment
import System.IO

import PMerge.Merge (merge)

type CurrentInput = [String]
type ReadState = StateT CurrentInput IO ()

data ReadHandleParams =
    ReadHandleParams { lineMVar :: MVar (Int, String)
                     , endMVar :: MVar IOException
                     , handleId :: Int
                     , handle :: Handle
                     }

updateLine :: Int -> String -> ReadState
updateLine which line = do
  state <- get
  let newInput = map
                 ( \(i,x) -> if i == which then line else x )
                 (zip [0..] state)
  put newInput
  return ()

printCurrentState :: ReadState
printCurrentState = do
  state <- get
  (liftIO . putStrLn) $ merge state
  return ()

readHandle :: ReadHandleParams -> IO ()
readHandle params = do
  let h = handle params
      i = handleId params
      l = lineMVar params
      e = endMVar params
  catch -- using ugly catch because "try" causes inconsistent type
        -- inference depending on whether i am using runghc or ghc!
    (do line <- hGetLine h
        putMVar l (i, line)
        readHandle params )
    (\x -> do hPutStrLn stderr $ "Error on handle #" ++ (show i) ++": " ++ (show x)
              putMVar e x)

mainLoop :: MVar (Int, String) -> ReadState
mainLoop lineMVar = do
   (handle, line) <- (liftIO . takeMVar) lineMVar
   updateLine handle line
   printCurrentState
   mainLoop lineMVar

startMainLoop :: MVar (Int, String) -> [String] -> IO ()
startMainLoop m i = do
  (state, result) <- runStateT (mainLoop m) i
  return ()

-- we open for writing so that we will always block
openPipe :: String -> IO (Handle,Handle)
openPipe f = liftM2 (,) (openFile f ReadMode) (openFile f WriteMode)

main :: IO ()
main = do
  args <- getArgs
  end <- newEmptyMVar
  line <- newEmptyMVar

  let startHandleReader :: Int -> Handle -> IO ThreadId
      startHandleReader id handle =
          forkIO $ (readHandle ReadHandleParams { lineMVar = line
                                                , endMVar = end
                                                , handleId = id
                                                , handle = handle
                                                })

  -- open all the pipes we need
  readWritePipes <- mapM openPipe args
  let pipes = map fst readWritePipes

  -- start consolidator
  hSetBuffering stdout LineBuffering
  hSetBuffering stdin LineBuffering
  mainLoopId <- forkIO $ startMainLoop line (replicate (1 + length pipes) "")

  -- start reading from stdin and all pipes
  stdinId <- startHandleReader 0 stdin
  readerIds <- forM (zip [1..] pipes) ( uncurry startHandleReader )

  takeMVar end
  mapM killThread (mainLoopId : stdinId : readerIds )
  mapM hClose (extract readWritePipes)
  hPutStrLn stderr "Exiting cleanly."
  return ()

extract :: [(a,a)] -> [a]
extract xs = xs >>= \x -> [fst x, snd x]
