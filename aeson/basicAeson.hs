{-# LANGUAGE OverloadedStrings, DeriveGeneric #-}


import Data.Text
import Network.HTTP.Conduit 
import GHC.Generics
import Data.Aeson

data Person = Person { firstName :: !Text
					 , lastName :: !Text 
	                 , age :: Int
	                 , likesPizza :: Bool
                      } deriving (Show,Generic)

instance FromJSON Person
instance ToJSON Person

-- Get JSON from some source

jsonURL = "http://daniel-diaz.github.io/misc/pizza.json"

getHTTPJson :: IO (Either String [Person])
getHTTPJson = eitherDecode <$> simpleHttp jsonURL

main :: IO ()
main =  getHTTPJson >>= (either putStrLn (mapM_ print))