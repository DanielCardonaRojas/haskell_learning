{-| 
Mostly math functions and some higher order combinators that will probably be 
use in future proyects.
-}
module GenericUtils where 




-- Evaluating math functions on ranges 
linspace lower upper num = takeWhile (<= upper) $ iterate (+ inc) lower where inc = abs (upper - lower) / (fromInteger num)

evalInRange lower upper f num = map f $ linspace lower upper num

evalInRange' = uncurry evalInRange


-- | within i s x es una funcion que prueba si x esta en el interval definido entre un limite inferior i y uno superior s
within l u x | l > u = within u l x
within l u x = 
	case (compare x l,compare x u) of 
		(LT,LT) -> LT
		(GT,GT) -> GT
		(_ , _) -> EQ 


within' l u = (== EQ) . within l u

withinCentered c l =  within (c - l) (c + l)  
       	   
withinCentered' c l = (== EQ) . withinCentered c l


-- mod like operation (wrap) that works on Floating types               	                	                   	 
modf x b = case within 0 b x of 
             EQ -> x
             LT -> modf (abs b + x) b
             GT -> modf (x - (abs b)) b

-- | constrain some times called clamp is function that forces a value to be in 
-- interval
constrain l u x = case within l u x of 
                     EQ -> x
                     LT -> l 
                     GT -> u    

-- | mapFromRange i o x positions x in interval o proportionally to its location on i.

mapFromRange (in_min,in_max) (out_min,out_max) x = 
	(x - in_min) * (out_max - out_min) / (in_max - in_min) + out_min 


-- Higher order functions and utilities

-- | Double composition operator apply a function that takes a single arg after a function that takes 2 args
-- For ex: a dotProduct using regular function composition would be written as
-- dotProduct xs = sum . zipWith (*) xs wich has type [a] -> [a] -> a with double composition dotProduct = sum ... zipWith (*)
(...) = (.) . (.)

-- | For more nemotecnic operators

(<..):: (c -> r) -> (a -> b -> c) -> (a -> b -> r)
(<..) = (.) . (.) 

(..>) :: (a -> s -> c) -> (c -> r) -> (a -> s -> r)
(..>) = flip (<..)

-- | Regular function composition
(<.) = (.)
-- | Reverse composition
(.>) = flip (.)

-- | fmap to a nested functor i.e f over IO (Maybe a) not to be mistake with liftA2 witch has a different type signature.
fmap2 :: (Functor f, Functor g) => (a -> b) -> f (g a) -> f (g b)
fmap2 = fmap . fmap 


toFloat n = fromIntegral n :: Float
toDouble n = fromIntegral n :: Double  

dotProduct = sum ... zipWith (*)   

rotate angle (x,y) = let rotationMatrix t =  [[cos t, negate $ sin t],[sin t, cos t]] 
                     in  zipWith dotProduct (rotationMatrix angle) (replicate 2 [x,y])     

