{-# LANGUAGE Arrows #-}
import Control.Arrow

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

-- Holes 

funWithHole :: String -> Int
-- String -> Int
funWithHole = (+ 1) . someFiltering
	where someFiltering = _


-- someFiltering = _









