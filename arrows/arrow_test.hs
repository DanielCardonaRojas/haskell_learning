{-# LANGUAGE Arrows #-}
import Control.Arrow
import Control.Category
import Control.Arrow.Operations --ArrowCircuit class

import Prelude hiding ((.), id)

f :: Int -> Int
f = arr (+ 3)

g :: Int -> Int
g = arr $ ((* 2) . (+ 1))

h = f &&& g 

j = f &&& g >>> unsplit (+)

-- j with defined with proc notation 
j' :: Int -> Int
j' = proc x -> do
		plus3 <- f -< x
		doublePlus1 <- g -< x
		returnA -< (plus3 + doublePlus1)

-- special operations/ combinators

split :: Arrow a => a b (b,b)
split = arr (\x -> (x,x))


unsplit :: Arrow k => (a -> b -> c) -> k (a,b) c 
unsplit = arr . uncurry

liftA2 :: (Arrow a) => (b -> c -> d) -> a e b -> a e c -> a e d
liftA2 op f g = f &&& g >>> unsplit op

-- (>>^) a f = a >>> arr f 

----------- KLEILI ARROWS ----------
--Kleisli m a b = Kleisli {runKleisli :: (a -> m b)} ([2,-2],[4])

plusminus :: Kleisli [] Int Int
plusminus = Kleisli (\x -> [x, -x])

double :: Kleisli [] Int Int
double = arr (* 2)

h2 :: Kleisli [] Int Int
h2 = liftA2 (+) plusminus double

h2Output = runKleisli h2 8

--IO Kleisli
test :: Kleisli IO String String
test = Kleisli putStrLn >>> Kleisli (const getLine) >>^ reverse


-- someFiltering = _

-- make an Arrow instance 

newtype SF a b = SF {runSF :: [a] -> [b]}



instance Category SF where
	id = SF (id :: [a] -> [a])
	(.) (SF g) (SF h) = SF (g . h)

instance Arrow SF where
	arr f = SF (map f) 
	-- first :: Arrow a => a b c -> a (b, d) (c, d)
	-- first :: SF a c -> SF (i, d) (o, d)
	first (SF f) = SF (unzip >>> toFst (f) >>> uncurry zip)


instance ArrowLoop SF where
	-- loop :: ArrowLoop a => a (b, d) (c, d) -> a b c
   loop (SF f) = SF $ \as ->
     let (bs,cs) = unzip (f (zip as (stream cs))) in bs
      where stream ~(x:xs) = x:stream xs

instance ArrowCircuit SF where
   delay x = SF (init . (x:))

toFst f (x,y) = (f x, y)

merge f = unsplit f

mergeA :: Arrow a => a (Double,Double) Double
mergeA = arr (uncurry (+))

-- iirpath :: Num a => [a] -> SF a a 
iirpath :: ArrowCircuit a => [Double] -> a Double Double
iirpath [] = arr (* 0)
iirpath (x:xs) = split >>> ((arr (*x)) *** (delay 1 >>> iirpath xs)) >>> mergeA

iir :: ArrowCircuit a => [Double] -> [Double] -> a Double Double
iir as bs = (iirpath as) >>> loop (mergeA >>> split >>> second (delay 0 >>> (iirpath bs)))

-- Automaton 

newtype Auto a b = A { getAuto :: a -> (b , Auto a b)} 

instance Category Auto where
	id = A $ (\a -> (a, id))
	(A h) . (A g) = A $ \x -> let 
		 					    (o1, g') = g x 
		 					    (o2, h') = h o1
						      in (o2, h' . g')
		
instance Arrow Auto where
	arr f = A (\x -> (f x, arr f)) 
	first (A f) = A $ \(x,y) -> let
									(o1,f') = f x 
									pairOut = (o1,y)
								in (pairOut, first f')

-- instance ArrowCircuit Auto where
-- 	delay = undefined 


runAuto :: Auto a b -> [a] -> [b]
runAuto _ [] = []
runAuto (A f) (x:xs) = 
	let
	  (o,f') = f x
	in o : runAuto (f') xs


testAutomaton = runAuto (arr (+1)) [1..5]

		    
