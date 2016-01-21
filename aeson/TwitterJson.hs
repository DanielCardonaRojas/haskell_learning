{-# LANGUAGE OverloadedStrings, DeriveGeneric #-}

import Data.Text (Text)
import Network.HTTP.Conduit
import GHC.Generics
import Data.Aeson
import Web.Authenticate.OAuth


myoauth :: OAuth
myoauth =
  newOAuth { oauthServerName     = "api.twitter.com/1.1/"
           , oauthConsumerKey    = "BpCEpv64uyRS29gvessSYEjba"
           , oauthConsumerSecret = "swEFsGMaACw1dMylaWDAeOrhyGsUvdqSIKBCAjPDexjoR65rPe"
             }

-- access and secret access okens
mycred :: Credential
mycred = newCredential "2857037969-idjjNVlMlpbXameEcDObVtioSQivFrk6Nr5mhQk"
                       "qVK5ugJlMuCqMzoi43JfzEUfHfPwhhCRqJRUbc1LyqrWe"

data Tweet =
  Tweet { text :: !Text
     
          } deriving (Show, Generic)                       

instance FromJSON Tweet
instance ToJSON Tweet

timeline :: String -> IO (Either String [Tweet])
timeline name = do 
		  req <- parseUrl $ "https://api.twitter.com/1.1/statuses/user_timeline.json?screen_name=" ++ name
		  -- Using a HTTP manager, we authenticate the request and send it to get a response.
		  res <- withManager $ \m -> do
		  	         signedreq <- signOAuth myoauth mycred req
		  	         httpLbs signedreq m
		  return (eitherDecode $ responseBody res)

main :: IO ()          
main = timeline "KikeCordobaC" >>=  either putStrLn (mapM_ print . take 5)




