import Data.Monoid
import Data.Foldable

data Flavor = 
	Sugar Grams 
	| Salt Grams 
	| Lemon Litres
	| Butter Grams 
	| Combination [Flavor] deriving (Show,Eq)

type Grams = Int 
type Litres = Int 

instance Monoid Flavor where
	mempty = Combination []
	mappend (Combination x) (Combination y) = Combination (x <> y)
	mappend (Combination xs) y = Combination (y:xs) 
	mappend y (Combination xs) = Combination (y:xs)
	mappend x y = Combination [x,y]


-- Maximum monoid 

data Max a = Max {getMax :: a} deriving (Show,Eq)

instance (Bounded a, Ord a) => Monoid (Max a)  where
	mempty = Max minBound
	mappend (Max x) (Max y) = Max $ max x y

data Min a = Min {getMin :: a} deriving (Show,Eq)

instance (Bounded a, Ord a) => Monoid (Min a)  where
	mempty = Min maxBound
	mappend (Min x) (Min y) = Min $ min x y

biggestLength ::[String] -> Int
biggestLength = getMax . foldMap (Max . length) 

smallestLength ::[String] -> Int
smallestLength = getMin . foldMap (Min . length)   

trimToShortest ls = map (take $ smallestLength ls) ls 