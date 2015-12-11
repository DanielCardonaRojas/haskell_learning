module CartaStyles (styleItemRecords, styleItemRecords3, styleCartaItems) where

{-#LANGUAGE OverloadedStrings, ExtendedDefaultRules #-}

import Lucid
import Data.Monoid
import Data.Text (pack,append) 
import qualified Data.Text.Lazy.IO as B
-- Custom Imports
import CartaTypes
import CsvUtils
import ListModifiers

{-
There is clearly something anoying about working with nested records and needing to repeat
code for these....

Something came to mind since HtmlT m () is a monad transformer. What about trying to use a Continuation monad.
-}
--Helpers

(<?>) :: Monoid b => (a -> b) -> Maybe a -> b
(<?>) = maybe mempty 

defItem = Item "Hamburguesa" (Just "Con tocineta y queso") "10.000"

--------------------------- Format -------------------------- 
formatPrice :: String -> String
formatPrice l | length l < 4 = "$" ++ l
formatPrice l = ("$" ++) . addDot . removeDollar  $ l
          where 
            removeDollar = filter (not . flip elem ("$. " :: String))
            addDot = reverse . insertAt 2 '.' . reverse


formatItem :: Item -> Item 
formatItem it = it {firstPrice = formatPrice $ firstPrice it} 

--How to transform a Maybe type into Html
instance ToHtml a => ToHtml (Maybe a) where
 	toHtml Nothing = mempty :: Monad m => HtmlT m () 
 	toHtml (Just x) = toHtml x :: Monad m => HtmlT m () 

 	toHtmlRaw Nothing = mempty :: Monad m => HtmlT m () 
 	toHtmlRaw (Just x) = toHtmlRaw x :: Monad m => HtmlT m ()  


comentedSection :: String -> Html ()
comentedSection sectionTitle =  "<!-- ****************" 
                                          <> (toHtml sectionTitle) <> "***************** -->" 


--------------------- Estilos para items de 1,2 o 3 precios --------------------

--Single item styles Sushi 7
itemStyle :: Item -> Html ()
itemStyle it = let i = formatItem it in do 
	div_ [class_ "menu-c"] $ do 
	   h3_ [class_ "carta-name"] (toHtml $ itemName i)
	   div_ [class_ "content-info-price"] $ do 
	   	(toHtml $ description i)
	   	span_ [class_ "price right"] (span_ (toHtml $ firstPrice i))

item2Style :: Item2 -> Html ()
item2Style i2 = let i = formatItem $ itemInfo i2 in 
	do
	div_ [class_ "menu-c"] $ do 
	   h3_ [class_ "carta-name"] (toHtml $ itemName i)
	   div_ [class_ "content-info-price"] $ do 
	   	(toHtml $ description i)
	   	span_ [class_ "price right"] $ do 
	   		span_ ((toHtml $ firstPrice i) <> " 1/2 " <> (toHtml $ secondPrice i2))




----------------------------- Column options ------------------------------
itemCartaStyle :: ItemCarta -> Html ()
itemCartaStyle (D1PItem i) = itemStyle i 
itemCartaStyle (D2PItem i) = item2Style i
itemCartaStyle (D3PItem i) = itemStyle $ (itemInfo . item2Info) i

twoItemRow :: ItemCarta -> ItemCarta -> Html ()
twoItemRow i i2 = nItemRow 2 [i,i2]

threeItemRow :: ItemCarta -> ItemCarta -> ItemCarta -> Html ()
threeItemRow i i2 i3 = nItemRow 3 [i,i2,i3]

nItemRow :: Int -> [ItemCarta] -> Html ()
nItemRow x is = 
	let 
	 padding m l = nextMultiple m l - l
	 numPadding = padding x (length is)
	in div_ [class_ "row"] $ do 
         mapM_ (div_ [class_ $ makeNRowClass x] . itemCartaStyle) (padAppending numPadding (last is) is)
      
makeNRowClass 3 = "large-4 medium-4 small-12 columns"
makeNRowClass 2 = "large-6 medium-6 small-12 columns"
makeNRowClass 1 = "large-12 medium-12 small-12 columns"
makeNRowClass _ = error "No se pueden tantas columnas"


nextMultiple n l = if mod l n == 0 then l else (div l n + 1) * n

------------------------------------ Exports ---------------------------------- 
-- | make a two column layout for the html
styleItemRecords :: [ItemCarta] -> Html ()
styleItemRecords is = (comentedSection "section") <> mapM_ (uncurry twoItemRow) (innerZip is) 

styleItemRecords3 is = (comentedSection "section") <> mapM_ (uncurry3 threeItemRow) (innerZip3 is)  

-- | Takes an int representing the number of columns a list Items and returns some Html
styleCartaItems :: Int -> [ItemCarta] -> Html () 
styleCartaItems n is = (comentedSection " section ") <> mapM_  (nItemRow n) (splitEvery n is)


