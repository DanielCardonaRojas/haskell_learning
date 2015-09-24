{-# LANGUAGE OverloadedStrings #-}

import Data.Text

-- Imports that will be needed later:
import qualified Data.Text.IO as T
import Data.Map as Map
import Control.Applicative


------------- IO with exception capabilities. ---------
data EitherIO e a = EitherIO {
    runEitherIO :: IO (Either e a)
}

----------- Functor, Applicative, Monad ----------------

instance Functor (EitherIO e) where
  fmap f = EitherIO . fmap (fmap f) . runEitherIO


instance Applicative (EitherIO e) where
  pure v = EitherIO $ return (Right v)
  -- (<*> :: IO (Either e (a -> b)) -> IO (Either e a) -> IO (Either e b)
  f <*> v = EitherIO $ (liftA2 (<*>)) ioFunc ioVal where
	          ioFunc = (runEitherIO f) -- is a function IO (Either e (a -> b))
	          ioVal = (runEitherIO v) -- is an IO (Either e a) value.
	          liftA2 func v1 v2 = fmap func v1 <*> v2
			  -- (liftA2 (<*>)) ioFunc ioVal == fmap (<*>) (ioFunc) <*> (ioVal)


instance Monad (EitherIO e) where
	return = pure
	x >>= f = EitherIO $ (runEitherIO x) >>= either (return . Left) (runEitherIO . f)

--------------- Helper Functions ---------------------

liftIO :: IO a -> EitherIO e a 
liftIO x = EitherIO $ (fmap Right) x

liftEither :: Either e a -> EitherIO e a 
liftEither v = EitherIO $ return v

throwE :: e -> EitherIO e a 
throwE = liftEither . Left

catchE :: EitherIO e a -> (e -> EitherIO c a) -> EitherIO c a
catchE value handler = EitherIO $ do 
		x <- runEitherIO value
		either (runEitherIO . handler) (return . Right) x  

------------------------- Logic ----------------------
data LoginError = InvalidEmail
                | NoSuchUser
                | WrongPassword
  deriving Show


getDomain :: Text -> Either LoginError Text
getDomain email =
  case splitOn "@" email of
    [name, domain] -> Right domain
    _              -> Left InvalidEmail 


printResult :: Either LoginError Text -> IO ()
printResult = T.putStrLn . either
  (const "ERROR: Invalid domain")
  (append "Domain: ")


getToken :: EitherIO LoginError Text
getToken = do
  liftIO (T.putStrLn "Enter email address:")
  input <- liftIO T.getLine
  liftEither (getDomain input)

-- runEitherIO getToken

users :: Map Text Text
users = Map.fromList [("example.com", "qwerty123"), ("localhost", "password")]

userLogin :: EitherIO LoginError Text
userLogin = do
  token      <- getToken
  userpw     <- maybe (throwE NoSuchUser)
                  return (Map.lookup token users)
  password   <- liftIO $ T.putStrLn "Enter your password:" >> T.getLine

  if userpw == password
     then return token
     else throwE WrongPassword

--runEitherIO userLogin



