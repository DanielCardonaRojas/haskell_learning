{-# LANGUAGE GeneralizedNewtypeDeriving #-}

import Control.Monad.Trans.Control
import Control.Monad.Base 
import Control.Monad.Reader 
import Control.Applicative
import Control.Monad.Trans





shouldIncrement :: Int -> Reader Bool Int
shouldIncrement x = do 
	should <- ask
	if should then return (x + 1) else  return x 


incrementOrDecrement :: Int -> Reader Bool Int
incrementOrDecrement x = do
	should <- ask
	if should then return (x + 1) else  return (x - 1) 

--runReader (shouldIncrement 1) True
--runReader (fmap (*3) shouldIncrement) True

shouldDouble :: Int -> Reader Bool Int
shouldDouble x = do 
	should <- ask 
	if should then return (x*2) else return (x)


zeroOne :: Reader Bool Int
zeroOne = reader (\x -> if x then 1 else 0)

compareToTen :: Reader Int Ordering
compareToTen = reader (compare 10) 


-- runReader (shouldDouble 1 >>= shouldIncrement) True

willDouble x = local (const True) (shouldDouble x)
decrementOrIncrement x = local (not) $ incrementOrDecrement x

test :: Reader Bool Int
test = do 
	x <- shouldIncrement 2
	y <- local not $ shouldIncrement x
	case even x of 
		True -> local (not) $ shouldDouble x
		False -> decrementOrIncrement x

------------ From Adit example -------

hello :: Reader String String
hello = do
    name <- ask
    return ("hello, " ++ name ++ "!")

bye :: Reader String String
bye = do
    name <- ask
    return ("bye, " ++ name ++ "!")

convo :: Reader String String
convo = do
    c1 <- hello
    c2 <- bye
    return $ c1 ++ c2

main = print . runReader convo $ "adit"


-----------------------------------------
toMaybe p x = if p x then Just x else Nothing

maybegreet :: Reader Bool (Maybe String)
maybegreet = do 
	should_greet <- ask
	if should_greet then  return (Just "Hello") else return Nothing

maybeCongratulate :: Reader Bool (Maybe String)
maybeCongratulate = reader (\env -> if not env then Just "Congratulations" else Nothing)


--greetCongratulate =  maybegreet >> maybeCongratulate
--runReader (runMaybeT (MaybeT maybegreet >> MaybeT maybeCongratulate)) True  



data Config = Config {name :: String , nickName :: Maybe String}

defaultConfig = Config "Daniel" (Just "Coco")

name' :: Reader Config String
name' = reader name

greet :: ReaderIO Config ()
greet = do
	c <- fmap name ask
	lift (putStrLn $ c ++ " ingresa una contrasena")
	x <- lift getLine
	lift $ putStrLn x	
	return ()

type ReaderIO a b = (ReaderT a IO b)	 























