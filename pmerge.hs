import Control.Concurrent
import qualified Control.Exception as E
import Control.Monad.State
import Data.Either
import System.Environment
import System.IO

type CurrentInput = [String]
type ReadState = StateT CurrentInput IO (String)

data ReadHandleParams =
    ReadHandleParams { lineMVar :: MVar (Int, String)
                     , endMVar :: MVar E.IOException
                     , handleId :: Int
                     , handle :: Handle
                     }


merge :: [String] -> String
merge xs = show (xs >>= (\x -> [":", x]))

updateLine :: Int -> String -> ReadState
updateLine which line = do
  state <- get
  let newInput = map
                 ( \(i,x) -> if i == which then line else x )
                 (zip [0..] state)
  put newInput
  return (merge newInput)

printCurrentState :: ReadState
printCurrentState = do
  state <- get
  let line = merge state
  (lift.putStrLn) line
  return line

readHandle :: ReadHandleParams -> IO ()
readHandle params = do
  let h = handle params
      i = handleId params
      l = lineMVar params
      e = endMVar params
  maybeLine <- E.try (hGetLine h)
  case maybeLine of
    Left x     -> putMVar e x
    Right line -> do
                   putMVar l (i, line)
                   readHandle params

mainLoop :: MVar (Int, String) -> ReadState
mainLoop lineMVar = do
   (handle, line) <- (lift . takeMVar) lineMVar
   updateLine handle line
   printCurrentState
   mainLoop lineMVar

startMainLoop :: MVar (Int, String) -> [String] -> IO ()
startMainLoop m i = do
  (state, result) <- runStateT (mainLoop m) i
  return ()

openPipe :: String -> IO Handle
openPipe f = openFile f ReadMode

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
  pipes <- mapM openPipe args

  -- start consolidator
  mainLoopId <- forkIO $ startMainLoop line (replicate (1 + length pipes) "")

  -- start reading from stdin and all pipes
  stdinId <- startHandleReader 0 stdin
  readerIds <- forM (zip [1..] pipes) ( uncurry startHandleReader )

  takeMVar end
  mapM killThread (mainLoopId : stdinId : readerIds )
  return ()
