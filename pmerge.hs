import System.IO
import Control.Monad.State
import Control.Concurrent

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
merge (master:rest) = master ++ (rest !! 0) -- XXX

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
  line <- hGetLine(handle params)
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

main :: IO ()
main = do
  end <- newEmptyMVar
  line <- newEmptyMVar
  pipe <- openFile "test.fifo" ReadMode
  forkIO (startMainLoop line ["",""])
  forkIO (readHandle ReadHandleParams { lineMVar = line
                                      , endMVar = end
                                      , handleId = 0
                                      , handle = stdin
                                      })

  forkIO (readHandle ReadHandleParams { lineMVar = line
                                      , endMVar = end
                                      , handleId = 1
                                      , handle = pipe
                                      })
  takeMVar end
  return ()
