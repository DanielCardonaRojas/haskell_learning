import Control.Concurrent
import Control.Monad

{-
Note: 

forkIO :: IO () -> IO ThreadId
forkIO is a function that takes a IO action and runs it concurrently in some thread
and returns the id of the created thread.

threadDelay :: Int -> IO ()
threadDelay is a function that delays the running thread for some amount of microSeconds

main1 and main2 should trully be tested compiling or doing runhaskell concurret_test.hs

Something key is to notice from main1 and main2 that threads will be killed if main returns
-}
main :: IO () 
main = test4 

test0 = forever $ do 
	s <- getLine
	forkIO $ setReminder s

test1 = do 
	s <- getLine
	if s == "exit"
		then return () 
		else forkIO (setReminder s) >> test1  

test2 = loop
 where
  loop = do
    s <- getLine
    if s == "exit"
       then return ()
       else do forkIO $ setReminder s
               loop          

oneSecond = 10 ^ 6

setReminder :: String -> IO ()
setReminder s = do 
	let t = read s :: Int
	putStrLn $ "Ok I'll remind you when in " ++ (show t) ++ " seconds "
	threadDelay (oneSecond * t) 
	thisId <- myThreadId
	putStrLn $ (show t) ++ " elapsed at thread " ++ (show thisId)


-- Communication between threads 
{-
MVar is the mechanism provided by haskell for comunicating between threads.
This is its API: 

data MVar a  -- abstract

newEmptyMVar :: IO (MVar a) 
newMVar      :: a -> IO (MVar a) 
takeMVar     :: MVar a -> IO a -- takeMVar waits for the MVar to be full and then extracts its value
putMVar      :: MVar a -> a -> IO ()

-}
--Example 1
test3 = do
  m <- newEmptyMVar
  forkIO $ (putStrLn "I will delay writing to an MVar" >> threadDelay oneSecond >> putMVar m 'x')
  r <- takeMVar m
  print r

test4 = do
  m <- newEmptyMVar
  forkIO $ do putMVar m 'x'; putMVar m 'y'
  r <- takeMVar m
  print r
  r <- takeMVar m
  print r

