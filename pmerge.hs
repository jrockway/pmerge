import Control.Concurrent
import Control.Monad.State
import System.Environment
import System.IO

data CurrentInput = CurrentInput { currentInput :: [String] }
                 deriving (Show)

type ReadState = StateT CurrentInput IO (String)

data ReadHandleParams =
    ReadHandleParams { lineMVar :: MVar (Int, String)
                     , endMVar :: MVar Bool
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
                 (zip [0..] (currentInput state))
  put $ CurrentInput { currentInput = newInput }
  return (merge newInput)

printCurrentState :: ReadState
printCurrentState = do
  state <- get
  let line = merge (currentInput state)
  (lift.putStrLn) line
  return line

readHandle :: ReadHandleParams -> IO ()
readHandle params = do
  line <- hGetLine (handle params)
  putMVar (lineMVar params) (handleId params, line)
  readHandle params

mainLoop :: MVar (Int, String) -> ReadState
mainLoop lineMVar = do
   (handle, line) <- (lift . takeMVar) lineMVar
   updateLine handle line
   printCurrentState
   mainLoop lineMVar

startMainLoop :: MVar (Int, String) -> [String] -> IO ()
startMainLoop m i = do
  (state, result) <- runStateT (mainLoop m) (CurrentInput { currentInput = i })
  return ()

openPipe :: String -> IO Handle
openPipe f = openFile f ReadMode

main :: IO ()
main = do
  args <- getArgs
  end <- newEmptyMVar
  line <- newEmptyMVar

  let startHandleReader :: Int -> Handle -> IO ()
      startHandleReader id handle = do
         forkIO $ readHandle
                ReadHandleParams { lineMVar = line
                                 , endMVar = end
                                 , handleId = id
                                 , handle = handle
                                 }
         return ()

  -- open all the pipes we need
  pipes <- mapM openPipe args

  -- start consolidator
  forkIO $ startMainLoop line (replicate (1 + length pipes) "")

  -- start reading from stdin and all pipes
  startHandleReader 0 stdin
  forM (zip [1..] pipes) ( \(i,h) -> startHandleReader i h )

  takeMVar end
  return ()
