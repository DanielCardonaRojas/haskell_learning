import Control.Monad
import Control.Applicative

{- 
There is no such thing as global vars in functional programming 
everyhthing has to be done withing a function. So the only way to 
do stateful computations is to make every function have an extra argument 
and return value (updated state).

Monad show useful once again in this case. So our first attempt would be something like
this. Make every function have an extra input argument for the state being passed
in and an extra return value: 

normalFunction :: a -> b
statefulFunction :: (a,s) -> (b,s)

But we can use currying to pass in a value followed by state just to be more
flexible.

statefulFuncion' :: a -> s -> (b,s)

We can go one step further by abstracting away functions that are purely stateful
i.e just depend on a state to give an output.

data PST s a = s -> (a,s) -- pure stateful computation

statefulFunction'' :: a -> PST s b

with this abstraction we can now think of (PST s) b as monadic values
and (a -> PST s b) as monadic functions

-}

data PST s a = PST { runStatefulComp:: s -> (a,s)}

-- Functor Instance
instance Functor (PST s) where
	-- fmap :: (a -> b) -> (PST s) a -> (PST s) b
	fmap f pst = PST $ \s -> let (v,s') = (runStatefulComp pst) s in (f v, s')

-- Applicative Instance
instance Applicative (PST s) where
	pure v = PST $ \s -> (v,s)
	-- (<*>) :: Applicative f => f (a -> b) -> f a -> f b
	(<*>) f v = PST $ \s -> 
			let 
			  (tf,s') = (runStatefulComp f) s
			  (res, s'') = (runStatefulComp v) s'
			in (tf res, s'')  


-- Monad Instance
instance Monad (PST s) where
	return v = PST $ \s -> (v,s)
	-- (>>=) :: (PST s) a -> (a -> (PST s) b) -> (PST s) b
	(>>=) x f =  PST $ \s -> let (v, s') = (runStatefulComp x) s 
					         in  runStatefulComp (f v) s'


getState :: PST s s 
getState =PST $ \s -> (s,s)

modifyState :: (s -> s) -> PST s ()
modifyState f = PST $ \s -> ((), f s)

putState s = modifyState (const s)

-- getState :: PST s a -> PST s s
-- getState ps = PST $ \s -> let (v,s') = runStatefulComp ps s 
--                           in (s', s')

-- -- modifyState :: (s -> d) -> PST s a -> PST d a
-- modifyState f pst = PST $ applyToSecond f . (runStatefulComp pst) 
-- 						where applyToSecond f (x,y) = (x, f y)


test0 = do 
	x <- getState
	if x > 0 then modifyState (+ 1) >> return True else modifyState (* 2) >> return False

t0 = runStatefulComp test0 3
