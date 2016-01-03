module CartaStyles 
      (styleCartaItems	
	   ) where

{-#LANGUAGE OverloadedStrings, ExtendedDefaultRules #-}

import Lucid
import Data.Monoid
import Data.Text (pack,append) 
import qualified Data.Text.Lazy.IO as B
-- Custom Imports
import CartaTypes
import CsvUtils
import ListModifiers
import Customizable

{-
There is clearly something anoying about working with nested records and needing to repeat
code for these....

Something came to mind since HtmlT m () is a monad transformer. What about trying to use a Continuation monad.

-}
--Helpers

(<?>) :: Monoid b => (a -> b) -> Maybe a -> b
(<?>) = maybe mempty 

comentedSection :: String -> Html ()
comentedSection sectionTitle =  "<!-- ****************" 
                                          <> (toHtml sectionTitle) <> "***************** -->" 

----------------------------- Column options ------------------------------
itemCartaStyle :: ToHtml m => m -> Html ()
itemCartaStyle = toHtml   

nItemRow :: ToHtml m => Int -> [m] -> Html ()
nItemRow 0 is = mapM_ itemCartaStyle is 
nItemRow x is = 
	let 
	 padding m l = nextMultiple m l - l
	 numPadding = padding x (length is)
	in div_ [class_ "row"] $ do 
         mapM_ (div_ [class_ $ makeNRowClass x] . itemCartaStyle) (padAppending numPadding (last is) is)
      
makeNRowClass 3 = "large-4 medium-4 small-12 columns"
makeNRowClass 2 = "large-6 medium-6 small-12 columns"
makeNRowClass 1 = "large-6 medium-12 small-12 large-centered columns"
makeNRowClass _ = error "No se pueden tantas columnas"


nextMultiple n l = if mod l n == 0 then l else (div l n + 1) * n

------------------------------------ Exports ---------------------------------- 
-- | 'styleCartaItems' __n is__ produces Html using the ToHtml instances of __is__ elements and  
-- gathering them in rows of n columns (Bootstrap or Zurb Foundation).
-- 
-- > Note: calling styleCartaItems 0 is will do no extra wrapping around the HTML produced by each item.
styleCartaItems :: ToHtml m => Int -> [m] -> Html () 
styleCartaItems n is = mapM_  (nItemRow n) (splitEvery n is)


