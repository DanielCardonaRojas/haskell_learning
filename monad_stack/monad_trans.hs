{-# LANGUAGE GeneralizedNewtypeDeriving #-}

import Control.Monad.Reader
import Control.Monad.State
import Control.Monad.Writer
import Control.Applicative


-- our monad transformer stack
--   a reader with an prefix string for the log
--      a state with the current value as integer
--         a writer to log the work done
--            all in a IO monad
type Prefix = String
type Counter = Int
type Log = [String]

-- defining a newtype and deriving from those various monad
-- type classes ease up the usage because the lift function
-- is not needed anymore.
-- attention, the GeneralizedNewtypeDeriving pragma is needed.
newtype MT a = MTa {
      runMTa :: ReaderT Prefix (StateT Counter (WriterT Log IO)) a
    } deriving ( Functor
               , Applicative 
               , Monad
               , MonadIO
               , MonadReader Prefix
               , MonadState Counter
               , MonadWriter Log)

-- count the number in the state down to 0 and
-- protocols every step in the writer. in doing
-- so, the prefix from the reader is prepended to
-- the logging string.
countDown :: MT ()
countDown = do n <- get
               modify (\x -> x - 1)
               n' <- get
               prefix <- ask
               let l = prefix ++ ": count from [" ++ show n ++ "] to: [" ++ show n' ++ "]"
               tell [l]
               liftIO $ putStrLn l
               if n' > 0
                   then countDown
                   else return ()

-- initialize our monad transformer stack with a prefix value and a value
-- from which we can count down and run a computation k inside this context.
runMT :: MT a -> Prefix -> Counter -> IO (a, Counter, Log)
runMT k prefix start = do ((a, c), l) <- runWriterT (runStateT (runReaderT (runMTa k) prefix) start)
                          return (a, c, l)

main = do
    putStrLn "start count down..."
    (_, c, l) <- runMT countDown "countDown" 10
    putStrLn "...count down finished"
    putStrLn ""
    putStr "the state value: "
    putStrLn $ show c
    putStrLn "the writer contains:"
    mapM_ putStrLn l


    