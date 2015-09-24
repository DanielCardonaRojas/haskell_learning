{-#LANGUAGE OverloadedStrings, ExtendedDefaultRules #-}

import Lucid
import Data.Text (pack)
import Data.Monoid 
import Control.Monad
import System.FilePath ((</>))
import Control.Monad.Reader
import Control.Applicative  
--((<$>),ZipList,(<*>))

------------------------ TYPES --------------------------
type URL = String
data Configuration = RowConfig {hasTitle :: Maybe String, hasReadMore :: Maybe URL}
----------------------- DEFAULTS ------------------------
defPath = "images/productos/sellos/"
defLink = "index.php/test"

mainSectConfig = RowConfig (Just "Title") (Just "productos/manuItem")
intSectConfig = RowConfig Nothing (Just "productos/manuItem")
--------------------- UTILITIES -------------------------
imgNamesToPaths images = map (defPath </>) images
namesFromImages images = map (fst . break (== '.')) images
makeNamePath nameList = zip (namesFromImages nameList) (imgNamesToPaths nameList)

dummyNamePaths = makeNamePath (map ((++ ".jpg").("Product" ++) . show) [1..6])

splitEvery _ [] = []
splitEvery n list = first : (splitEvery n rest)
  where
    (first,rest) = splitAt n list

------------------------------------------------------------
--------------------------- LUCID --------------------------
------------------------------------------------------------


itemRowC :: Configuration -> [(String,FilePath)] -> Maybe (Html ())
itemRowC (RowConfig t r) nameAndPaths = fmap (div_ [class_ "row"]) (
								((h3_ . toHtml) <$> t) <> 
								Just (mapM_ (uncurry item) nameAndPaths) <>
								(readMore <$> r)	
								)

configuredSection :: Configuration -> [(String, FilePath)] -> Maybe (Html ())
configuredSection config nameAndPaths = 
	let 
		groupsOf3 = splitEvery 3 nameAndPaths
		htmlRows = map (itemRowC config) groupsOf3
	in mconcat htmlRows


configuredSections :: [Configuration] -> [(String, FilePath)] -> Maybe (Html ())
configuredSections configs nameAndPaths = 
	let 
		groupsOf3 = splitEvery 3 nameAndPaths
		htmlRows = ZipList (itemRowC <$> configs) <*> (ZipList groupsOf3)
	in mconcat (getZipList htmlRows)

---------------------- basic ----------------

item :: String -> FilePath -> Html ()
item title imgPath = 
	div_ [class_ "col-md-4"] $ do
		a_ [href_ (pack imgPath),target_ "_blank", type_ "image/jpeg", class_ "jcepopup noicon"] $ 
			img_ [href_ (pack imgPath), alt_ (pack title), width_ "100%"] 
		p_ [] (toHtml title)


readMore :: String -> Html ()
readMore link = p_ [class_ "readmore"] $ a_ [href_ (pack link)] (toHtml "Ver Más")	 	

------------------------------ IO () --------------------------

main :: IO ()
main = do 
	fileNames <- fmap (lines) $ readFile "listOfImages.txt"
	let npths = makeNamePath fileNames
	let maybeHtml = configuredSection mainSectConfig npths
	case maybeHtml of 
		Nothing -> putStrLn "Error Procesando informacion"
		Just x -> renderToFile "section.html" (x)
	
