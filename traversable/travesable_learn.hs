import Control.Applicative
import Data.Monoid
import Data.Traversable
import Data.Foldable
import Control.Monad.Trans.Maybe
import Control.Monad.Trans
import Control.Monad.Reader
import Text.Read

data Max = Max {getMax :: Int} | MinusInfinity deriving (Show)
data MyTree a = Empty | Leaf a | Node a (MyTree a) a deriving (Show)


instance Monoid Max where
	mempty = MinusInfinity
	mappend a MinusInfinity = a
	mappend MinusInfinity a = a
	mappend (Max a) (Max b) = Max (max a b) 


instance Functor MyTree where
	fmap f Empty = Empty
	fmap f (Leaf x) = Leaf (f x)
	fmap f (Node a t b) = Node (f a) (fmap f t) (f b) 


instance Foldable MyTree where
		-- foldMap :: Monoid m => (a -> m) -> MyTree a -> m
		foldMap _ Empty = mempty
		foldMap m (Leaf a) = m a
		foldMap m (Node a t b) = m a <> foldMap m t <> m b


instance Traversable MyTree where
	-- traverse :: Applicative f => (a -> f b) -> MyTree a -> f (MyTree b)
	traverse f Empty = pure Empty
	traverse f (Leaf x) = Leaf <$> f x
	traverse f (Node a t b) = Node <$> (f a) <*> (traverse f t) <*> (f b)

-- TEsting tree

defTree = Node "daniel" (Node "pedro" (Leaf "Zacarias") "Emili") "Sara"

--Testing Traversable
printGetLine :: (Show a) => a -> IO (String)
printGetLine x = print x >> getLine 

correctUserName :: String -> MaybeT IO String
correctUserName x = do
	liftIO (putStrLn $ "Enter user name for : " ++ (x))
	u <- liftIO getLine
	let val = lookup u [("daniel","cocox6"),("pedro","malangas")]
	MaybeT $ return val
	-- if u == "coco" then return u else  MaybeT $ return Nothing


correctPass :: String -> MaybeT IO String
correctPass x = do
	liftIO (putStrLn $ "Enter pass for username: " ++ (x))
	u <- liftIO getLine
	let val = lookup u [("cocox6","corosito48"),("malangas","pedroJames44")]
	MaybeT $ return val


correctLength :: String -> ReaderT Int IO String
correctLength s = do 
	liftIO $ putStrLn s 
	let l = length s 
	l' <- ask
	case compare l' l of
		GT -> liftIO (putStrLn "Is a longer word but ok..." >> return s)
		EQ -> liftIO (putStrLn "Just the right amount of characters" >> return s)
		LT -> liftIO $ do 
			      putStrLn $ "Enter a word that has at least" ++ (show l)
			      getLine


test0 = traverse printGetLine defTree
test0List = traverse printGetLine [1..3]
test0Trans = runMaybeT $ traverse (\x -> correctUserName x >>= correctPass) ["daniel", "pedro"]
test1Trans = runMaybeT $ traverse (\x -> correctUserName x >>= correctPass) defTree

test2Trans = runReaderT (traverse correctLength defTree) 9
--Test Foldable

test1 = foldMap (All . (> 3). length) defTree

test2 = foldMap (Max . length) defTree

--this is a filter like operation returns a list, dont know how filter is actually implemented 

filterTree ::(Foldable t,Functor t) => (a -> Bool) -> t a -> [a]
filterTree p t = fold (fmap (\x -> if p x then [x] else []) t) 

test3 = filterTree ((> 3) . length) defTree


{-Lets test foldM a little
Remember
foldM :: (Monad m, Foldable t) => (b -> a -> m b) -> b -> t a -> m b 

Especialized to list

foldM :: (Monad m) => (b -> a -> m b) -> b -> [a] -> m b
-}
data Color = Red | Green | Blue | Yellow | White | Orange deriving (Show,Eq,Read)

fun :: String -> Color -> IO (String)
fun s c = do 
	putStrLn $ "What is your favorite color besides: " ++ s
	ans <- getLine
	let x = (readMaybe::String -> Maybe Color) ans
	case fmap (== c) x of
		Just True -> putStrLn "Wow man just the color I was thinking about!" >> return ans
		Just False -> putStrLn  ("I honestly prefer: " ++ (show c))  >> return ans
		Nothing -> putStrLn "Man thats not a color I've heard of!"	>> return ans


test4 = foldM fun "white" [Red,Blue,Green,Yellow]

-- Could a generalized filtering operation be written as: 

gfilt p xs = foldMap (\x-> if p x then [x] else []) xs
