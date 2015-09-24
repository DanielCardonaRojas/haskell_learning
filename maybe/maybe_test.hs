import Control.Monad.Trans.Maybe
import Control.Monad.Trans
import Control.Monad.State
{-
Notes: 
	i) Using MaybeT (State s) a yeilds: s -> (Maybe a, s)
	ii) Using StateT s (Maybe) a yeilds: s -> Maybe (a,s)


-}
liftMaybe :: Monad m => Maybe a -> MaybeT m a
liftMaybe v = MaybeT $ return v  

test1 :: MaybeT (State Int) (String)
test1 = do
  x <- lift get
  if even x then liftMaybe Nothing
  else lift $ modify (+ 1) >> return ("Hola")
-- runState (runMaybeT test1) 2

test1' :: String -> MaybeT (State Int) (String)
test1' s = do
  modify (+ 1)
  return (s ++ " Daniel")

-- runState (runMaybeT (test1 >>= test1')) 4
-- runState (runMaybeT (test1 <|> test1' "Hello ")) 4


