{- 

Lens short tutorial 

Goals : 
	Represent a setter and a getter in one data type.
	Solve the problem of digging down a nested record.

first attempt: 

NaiveLens s a = NaiveLens { getter :: s -> a
	                      , setter :: a -> s -> s 
                          }

Downside : to modify a focused value we would have to first get then apply a function and then set.                         


NaiveLens' s a = NaiveLens' { getter :: s -> a
	                        , over :: (a -> a) -> s -> s 
                            }
in this case set v = over (const v), we can see over gives a little more flexibility               

To express both getter and setter in a single type (the over function must be generalized)

Lens s a = (Functor f) => (a -> f a) -> (s -> f s)

Think of a lens as a type that encodes both over,set and get. So we will have to write helper functions 
to extract any of these.


-}

{-#LANGUAGE RankNTypes #-}

import Data.Functor.Identity
import Control.Applicative

type Lens s a = (Functor f) => (a -> f a) -> s -> f s

over :: Lens s a -> (a -> a) -> s -> s
over ln f s = runIdentity $ (ln $ Identity . f) s 

set :: Lens s a -> a -> s -> s 
set ln a s = over ln (const a) s




get :: Lens s a -> s -> a 
get ln s = getConst $ (ln Const) s 



-- Test on a data type
data Person = Person {name :: String, age :: Int} deriving (Show,Eq)

setName :: Person -> String -> Person
setName p newname = p {name = newname}


-- Creating a lens isnt that obvious 
{-
nameLens m rec 

m :: (a -> f a)
rec :: s

To apply m first extract 'a' out of s: 

m (name s) :: f a

now to turn (f a) into (f s) we have to fmap something of type (a -> s)

(a -> s) is a setter

--

Now if (f = Identity) then  we will transform s : fmap setName (Identity oldName)
if (f = Const) then fmap (setName s) (Const OldName) 
-}

nameLens m s = fmap (setName s) (m $ name s) 

somePerson = Person "Daniel" 25





