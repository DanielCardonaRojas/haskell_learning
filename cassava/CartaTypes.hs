{-# LANGUAGE OverloadedStrings #-}

import Data.Either
import CsvUtils
import Data.Csv

data Person = Person {name :: !String, ident :: !String} deriving (Show, Eq)

instance FromNamedRecord Person where
    parseNamedRecord r = Person <$> r .: "name" <*> r .: "salary" 

instance FromRecord Person where
        parseRecord r = Person <$> (r .! 0) <*> (r .! 1)              

--Try switching the data type for ident  field
data Plato = Plato
    { platoBasic :: Item
    , platoPrice :: String
    } deriving (Eq, Show)

data Item = Item
    { itemName :: String
    , description :: Maybe String
    , price :: String 
    } deriving (Eq, Show)

-- INSTANCES for parsing CSV data
instance FromNamedRecord Item  where
    parseNamedRecord r = Item <$> (r .: "nombre") <*> (r .: "descripcion") <*> (r .: "precio")

instance FromRecord Item where
    parseRecord r = Item <$> (r .! 0) <*> (r .! 1) <*> (r .! 2)


