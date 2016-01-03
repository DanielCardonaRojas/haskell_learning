module Zippable where
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
import Control.Monad.Writer
import Control.Monad
import Data.Monoid 
import Control.Applicative
import Data.Tree

{- 
Experiment on Zippable typeclass 

zip :: [a] -> [b] -> [(a,b)]
zipWith :: (a -> b -> c) -> [a] -> [b] -> [c]

Couldn't this idea be generalized to other containter types besides from lists?
	e.x: zipWithMay :: (a -> b -> c) -> Maybe a -> Maybe b -> Maybe c
	     zipWithTree :: (a -> b -> c) -> Tree a -> Tree b -> Tree c

Isn't there an almost obvious relationship with Applicative ?
  <*> :: f (a->b) -> f a -> f b

-}
uncurry3 :: (a -> b -> c -> d) -> (a,b,c) -> d
uncurry3 f = \(x,y,z) -> f x y z

class Applicative z => Zippable z where
	zip' :: z a -> z b -> z (a,b)
	zip' = zipWith' (,)

	zip3' :: z a -> z b -> z c -> z (a,b,c)
	zip3' = zipWith3' (,,)

	zipWith' ::  (a -> b -> c) -> z a -> z b -> z c
	zipWith' = liftA2 

	zipWith3' ::  (a -> b -> c -> d) -> z a -> z b -> z c -> z d
	zipWith3' = liftA3


-- | Just use default instance, i.e do not overide methods.
instance Zippable Maybe 
instance Zippable IO 


-- | Zippable class can be derived with language extension GeneralizedNewTypeDeriving.
newtype Optional a = Optional { unOptional :: Maybe a} deriving (Eq, Show,Zippable,Applicative,Functor)
optJust = Optional . Just 
optNothing = Optional Nothing 
		         

--------------- Tree instance -------------	         

-- | Overides default implementation for zipWith'
instance Zippable Tree where
	zipWith' f (Node a []) (Node b _) = Node (f a b) []
	zipWith' f (Node a _) (Node b []) = Node (f a b) []
	zipWith' f (Node a as) (Node b bs) = Node (f a b) (map (uncurry $ zipWith' f) (zip as bs))

	zipWith3' f (Node a []) (Node b _) (Node c _) = Node (f a b c) []
	zipWith3' f (Node a _) (Node b []) (Node c _) = Node (f a b c) []
	zipWith3' f (Node a _) (Node b _) (Node c []) = Node (f a b c) []
	zipWith3' f (Node a as) (Node b bs) (Node c cs) = Node (f a b c) (map (uncurry3 $ zipWith3' f) (zip3 as bs cs))

------------- List instance ------------------
instance Zippable ZipList

-- | This should work identical to Preludes Zip Functions, so just use ZipList instance
instance Zippable [] where
	zipWith' f l k = getZipList $ zipWith' f (ZipList l) (ZipList k)
	zipWith3' f l k j = getZipList $ zipWith3' f (ZipList l) (ZipList k) (ZipList j)

---------------- EXAMPLES ------------------

rojasTree = Node "Familia Rojas" 
             [
               Node "Claudia" [], 
               Node "Raul" [Node "Wilson" []]
             ]

cardonaTree = Node "Familia Cardona"
                [
                  Node "Diego" [],
                  Node "Mauricio" [],
                  Node "Juan" []
                ]            

loves x y = x ++ " loves " ++ y

testZipTree = mapM_ print (zipWith' loves rojasTree cardonaTree)

getuserInt = fmap (read :: String -> Int) getLine
testIOZipWith = zipWith' (+) (getuserInt) (putStrLn "Enter a second number" >> getuserInt)
testIOZip = zip' getLine getuserInt


