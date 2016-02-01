{- | 

En este modulo se describen los tipos bases para describir un elemento de una carta de 
restaurante. Por simplicidad se ha definido que un elemento de carta tiene nombre, posiblemente 
descripcion y al menos un precio. Para otro tipo de elementos con mas precios como para 
describir un trago en la seccion de licores se ha dispuesto dos tipos mas que pueden tener dos 
o tres precios.

-}
module CartaTypes ( ItemCarta (..)
                  , Item (..)
                  , Item2 (..)
                  , Item3 (..)
                  , PricedOnce (..)
                  ,PricedTwice (..)
                  ,NamedItem (..)) where

{-# LANGUAGE OverloadedStrings #-}

import Data.Either
import CsvUtils
import Data.Csv
import Control.Applicative 
import Data.ByteString.Char8
import Data.ByteString.Lazy.Char8


-- | Un item de carta es un item descrito con 3,2 o 1 precio. 
data ItemCarta = D3PItem Item3 | D2PItem Item2 | D1PItem Item deriving (Eq, Show)


data Item = Item
    { itemName :: String
    , description :: Maybe String
    , firstPrice :: String 
    } deriving (Eq, Show)

data Item2 = Item2
    { itemInfo :: Item
    , secondPrice :: String 
    } deriving (Eq, Show)

data Item3 = Item3
    { item2Info :: Item2
    , thirdPrice :: String 
    } deriving (Eq, Show)


-- INSTANCES for parsing CSV data
instance FromNamedRecord Item  where
    parseNamedRecord r = Item <$> (r .: "nombre") <*> (r .: "descripcion") <*> (r .: "precio")

instance FromRecord Item where
    parseRecord r = Item <$> (r .! 0) <*> (r .! 1) <*> (r .! 2)

instance FromRecord Item2 where
    parseRecord r = Item2 <$> (parseRecord r) <*> (r .! 3)

instance FromNamedRecord Item2 where
     parseNamedRecord r = Item2 <$> (parseNamedRecord r) <*> (r .: "precio_2")

instance FromRecord Item3 where
    parseRecord r = Item3 <$> (parseRecord r) <*> (r .! 4)

instance FromNamedRecord Item3 where
     parseNamedRecord r = Item3 <$> (parseNamedRecord r) <*> (r .: "precio_3")

--Does Does Parser instance of Alternative rewind string if failure ? <|>
instance FromRecord ItemCarta where
    parseRecord r = (D3PItem <$> (parseRecord r)) <|> (D2PItem <$> (parseRecord r)) 
                                  <|> (D1PItem <$> (parseRecord r)) 

instance FromNamedRecord ItemCarta where
    parseNamedRecord r = (D3PItem <$> (parseNamedRecord r)) <|> (D2PItem <$> (parseNamedRecord r)) 
                                  <|> (D1PItem <$> (parseNamedRecord r)) 


-- Utility Classes
-- | Un item o elemento de la carta/menu que tiene al menos un primer precio
class PricedOnce a where 
    priceOne :: a -> String

-- | Un item o elemento de la carta/menu que tiene al menos un primer precio
class PricedTwice a where
    priceTwo :: a -> String


class NamedItem a where
    itemsName :: a -> String
    itemsDescription :: a -> Maybe String


instance PricedOnce Item where
    priceOne = firstPrice


instance PricedOnce Item2 where
    priceOne = firstPrice . itemInfo 


instance PricedOnce Item3 where
    priceOne = firstPrice . itemInfo . item2Info

instance PricedTwice Item2 where
    priceTwo = secondPrice

instance PricedTwice Item3 where
    priceTwo = secondPrice . item2Info

instance NamedItem Item where
    itemsName = itemName
    itemsDescription = description

instance NamedItem Item2 where
    itemsName = itemName . itemInfo
    itemsDescription = description . itemInfo

instance NamedItem Item3 where
    itemsName = itemName . itemInfo . item2Info
    itemsDescription = description . itemInfo . item2Info 





