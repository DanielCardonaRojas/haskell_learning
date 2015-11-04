import Control.Monad
import Control.Concurrent

{-
A logging service is a thread to which the rest of the program 
can send messages, and it is the job of the logger to record those
 messages somewhere.

The API of the Logger is 

initLogger :: IO Logger
logMessage :: Logger -> String -> IO ()
logStop    :: Logger -> IO ()

-}

data Logger = Logger (MVar LogCommand)
data LogCommand = Message String | Stop (MVar ())

initLogger :: IO Logger
initLogger = do
  m <- newEmptyMVar
  let l = Logger m
  forkIO (logger l)
  return l

logger :: Logger -> IO ()
logger (Logger m) = loop
 where
  loop = do
    cmd <- takeMVar m
    case cmd of
      Message msg -> do
        putStrLn msg
        loop
      Stop s -> do
        putStrLn "logger: stop"
        putMVar s ()

logMessage :: Logger -> String -> IO ()
logMessage (Logger m) s = putMVar m (Message s)

logStop :: Logger -> IO ()
logStop (Logger m) = do
  s <- newEmptyMVar
  putMVar m (Stop s)
  takeMVar s

oneSecond = 10 ^ 6

main :: IO ()
main = do
  l <- initLogger
  logMessage l "hello"
  forkIO $ (threadDelay oneSecond >>logMessage l "Daniel")
  logMessage l "bye"
  getLine >>= \x -> if x == "exit" then logStop l else return ()
  