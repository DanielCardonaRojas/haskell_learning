{-# LANGUAGE OverloadedStrings, DeriveGeneric #-}

import Data.Text (Text,append,unpack)
import Network.HTTP.Conduit
import GHC.Generics
import Data.Aeson
import Control.Monad
import Web.Authenticate.OAuth

{-
Always enable top language extensions in ghci

Notes 

simpleHttp :: String -> IO ByteString
-}

data SpreadSheetEntry = SpreadSheetEntry { title :: !Text
										 , content :: !Text
										 } deriving (Show,Generic)
--instance FromJSON SpreadSheetEntry

instance FromJSON SpreadSheetEntry where  
    parseJSON (Object v) = do 
    	v .: "entry" 
    	SpreadSheetEntry <$> v .: "title" <*> v .: "content"                         
    	
    parseJSON _ = mzero                 

instance ToJSON SpreadSheetEntry

requestURL :: String -> String
requestURL spreadID = "https://spreadsheets.google.com/feeds/list/" ++ spreadID ++ "/1/public/basic?alt=json"
																

somePublicSpreadSheet = "17YlPBFCq49O5p_BgsfESHoQyi0cJyM-P7z6tHfc1sjs"

getJson :: IO (Either String SpreadSheetEntry)
getJson = eitherDecode <$> simpleHttp (requestURL somePublicSpreadSheet)

--main = getJson >>= either putStrLn (mapM_ print)