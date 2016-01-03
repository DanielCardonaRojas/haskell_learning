import Control.Monad.Trans
import Control.Monad.Cont
import Lucid 

{- 
A continuation is a Cont r a is a computation that will deliver an r after a future function (a -> r) is supplied.

In the context of continuations what one does is feed the future function (callback) with modified values. i.e 
we do some partial work and pass on the the completed work to the future computation.

newtype Cont r a = Cont {runCont (a -> r) -> r}

All that needs to be done in a continuation to get value of type 'r' is supply a function (a -> r)

The most abstract type in Cont module is:

newtype ContT r (m :: * -> *) a
  = ContT {runContT :: (a -> m r) -> m r}

THis is justa generalization to include anoother monadic effect m inside the contination.

-}

-- Start by specializing the type: 
type ContIO r a  = ContT r IO a -- ^ ContIO is a continuation that will deliver an IO r

getLineCont = liftIO getLine :: ContIO r String

putStrLnCont :: String -> ContIO r () 
putStrLnCont s = liftIO $  putStrLn  s 

revName x = putStrLn "Your reversed name is: " >> (return $ reverse x)

test0 = runContT (putStrLnCont "Enter your name" >> getLineCont) revName
            






