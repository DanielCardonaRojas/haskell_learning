{-# LANGUAGE OverloadedStrings, DeriveGeneric #-}


import Data.Text
import Network.HTTP.Conduit 
import GHC.Generics
import Data.Aeson


data Verse = Verse { bookname :: !Text
				   , chapter :: !Text
				   , verse :: !Text
				   , text :: !Text
	
                   } deriving (Show,Generic)

instance ToJSON Verse
instance FromJSON Verse

requestURL = "http://labs.bible.org/api/?passage=random&type=json"

getDailyVerse :: IO (Either String [Verse])
getDailyVerse = eitherDecode <$> (simpleHttp requestURL)

main = getDailyVerse  >>= either putStrLn (mapM_ print)              