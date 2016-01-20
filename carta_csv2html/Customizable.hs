module Customizable where

{-#LANGUAGE OverloadedStrings, ExtendedDefaultRules #-}

import Lucid
import Data.Monoid
import Data.Text (pack,append) 
import CartaTypes
import ListModifiers

-------------------- Helpers ------------------

description' :: Monad m => Item -> HtmlT m ()
description' = toHtml . description

itemName' :: Monad m => Item -> HtmlT m ()
itemName' = toHtml . itemName

firstPrice' :: Monad m => Item -> HtmlT m ()
firstPrice' = toHtml . formatPrice . firstPrice

secondPrice' :: Monad m => Item2 -> HtmlT m ()
secondPrice' = toHtml . formatPrice . secondPrice

thirdPrice' :: Monad m => Item3 -> HtmlT m ()
thirdPrice' = toHtml . formatPrice . thirdPrice
--------------------------- Format -------------------------- 
formatPrice :: String -> String
formatPrice l | length l < 4 = "$" ++ l
formatPrice l = ("$" ++) . addDot . removeDollar  $ l
          where 
            removeDollar = filter (not . flip elem ("$. " :: String))
            addDot = reverse . insertAt 2 '.' . reverse


formatItem :: Item -> Item 
formatItem it = it {firstPrice = formatPrice $ firstPrice it} 

--------------------------- Item Styles ------------------------
itemStyle :: Monad m => Item -> HtmlT m ()
itemStyle it = let i = formatItem it in do
	div_ [class_ "menu-c"] $ do 
	   h3_ [class_ "carta-name"] (itemName' i)
	   div_ [class_ "content-info-price"] $ do 
	   	(description' i) <> (firstPrice' i)

-- item2Style :: Item2 -> Html ()
item2Style :: Monad m => Item2 -> HtmlT m ()
item2Style i2 = let i = formatItem $ itemInfo i2 in do
	div_ [class_ "row"] $ do 
	  div_ [class_ "large-4 columns"] $ h5_ [class_ "carta-titulo"] (itemName' i)
	  div_ [class_ "large-4 columns"] $ span_ [class_ "carta-descripcion"] (description' i)
	  div_ [class_ "large-2 columns"] $ span_ [class_ "carta-precio"] (firstPrice' i)
	  div_ [class_ "large-2 columns"] $ span_ [class_ "carta-precio"] (secondPrice' i2) 

-- item3Style :: Item3 -> Html ()
item3Style :: Monad m => Item3 -> HtmlT m ()
item3Style i3 = let i2 = item2Info i3; i = formatItem $ itemInfo i2 in do
	div_ [class_ "row"] $ do 
	  div_ [class_ "large-3 columns"] $ h5_ [class_ "carta-titulo"] (itemName' i)
	  div_ [class_ "large-3 columns"] $ span_ [class_ "carta-descripcion"] (description' i)
	  div_ [class_ "large-2 columns"] $ span_ [class_ "carta-precio"] (firstPrice' i)
	  div_ [class_ "large-2 columns"] $ span_ [class_ "carta-precio"] (secondPrice' i2) 
	  div_ [class_ "large-2 columns"] $ span_ [class_ "carta-precio"] (thirdPrice' i3) 


----------------------- Convert to Html ---------------------

--How to transform a Maybe type into Html
instance ToHtml a => ToHtml (Maybe a) where
 	toHtml Nothing = mempty :: Monad m => HtmlT m () 
 	toHtml (Just x) = toHtml x :: Monad m => HtmlT m () 

 	toHtmlRaw Nothing = mempty :: Monad m => HtmlT m () 
 	toHtmlRaw (Just x) = toHtmlRaw x :: Monad m => HtmlT m ()  

--------------------- Deafult conversion from ItemCarta to html ------------

instance ToHtml ItemCarta where
	toHtml m = case m of 
		          D1PItem i -> itemStyle i 
		          D2PItem i -> item2Style i
		          D3PItem i -> item3Style i 

	toHtmlRaw = undefined
-- Style Restaurante Ank 
newtype AnkItemCarta = AnkItemCarta { ankItemCarta :: ItemCarta} deriving (Eq, Show)
instance ToHtml AnkItemCarta where
	toHtml (AnkItemCarta m) = case m of 
		          D1PItem i -> do
		               dl_ $ do 
		               	  dt_ (itemName' i)
		               	  dd_ [class_ "price"] (priceOne i)
		               div_ [class_ "carta-txt"] $ do 
		               	  span_ [class_ "carta-span"] (description' i)    
                     	          	



		          D2PItem i -> undefined
		          D3PItem i -> undefined
	toHtmlRaw _ = undefined 


-- Style Brasas 
newtype BrasasItemCarta = BrasasItemCarta { brasasItemCarta :: ItemCarta} deriving (Eq, Show)
instance ToHtml BrasasItemCarta where
	toHtml (BrasasItemCarta m) = case m of 
		          D1PItem i -> undefined
		          D2PItem i -> undefined
		          D3PItem i -> undefined
	toHtmlRaw _ = undefined 

-- Style Sushi7
newtype Sushi7ItemCarta = Sushi7ItemCarta { sushi7ItemCarta :: ItemCarta} deriving (Eq, Show) 
instance ToHtml Sushi7ItemCarta where
	toHtml (Sushi7ItemCarta m) = case m of 
		          D1PItem i -> undefined
		          D2PItem i -> undefined
		          D3PItem i -> undefined
	toHtmlRaw _ = undefined 

-- Style Wajaca
newtype WajacaItemCarta = WajacaItemCarta { wajacaItemCarta :: ItemCarta} deriving (Eq, Show) 
instance ToHtml WajacaItemCarta where
	toHtml (WajacaItemCarta m) = case m of 
		          D1PItem i -> undefined
		          D2PItem i -> undefined
		          D3PItem i -> undefined
	toHtmlRaw _ = undefined 

-- Style Wajaca
newtype VillageItemCarta = VillageItemCarta { villageItemCarta :: ItemCarta} deriving (Eq, Show) 
instance ToHtml VillageItemCarta where
	toHtml (VillageItemCarta m) = case m of 
		          D1PItem i -> undefined
		          D2PItem i -> undefined
		          D3PItem i -> undefined
	toHtmlRaw _ = undefined 