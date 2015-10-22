import Data.Monoid

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