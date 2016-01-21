{-# LANGUAGE OverloadedStrings, DeriveGeneric #-}


import Data.Text
import Network.HTTP.Conduit 
import GHC.Generics
import Data.Aeson

data Conversion = Conversion { source :: !Text
						   , target :: !Text
						   , amount :: Double
	
                           } deriving (Show,Generic)
instance FromJSON Conversion
instance ToJSON Conversion

getConversionURL :: Text -> Text -> Text
getConversionURL from to = "https://currency-api.appspot.com/api/" +.+ from +.+ "/" +.+ to +.+ ".json" where (+.+) = append                          

getConversion :: Text -> Text -> Double -> IO (Maybe Conversion)
getConversion from to amt = (fmap . fmap ) incrementByAmount (decode <$> simpleHttp url) 
                   where 
                   	 url = unpack $ getConversionURL from to  
                   	 incrementByAmount conv = conv {amount = (* amt) $ amount conv}


main = putStrLn "BitCoin to USD Converter" >> getConversion "BTC" "USD" 2