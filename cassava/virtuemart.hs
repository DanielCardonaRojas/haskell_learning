{-# LANGUAGE ScopedTypeVariables, OverloadedStrings #-}

import Data.Traversable 
import Control.Applicative
import Data.Csv
import qualified Data.Vector as V
import qualified Data.ByteString.Lazy as BL
import Control.Monad
import Data.Monoid
import Data.Tree
import Data.List 

import Data.ByteString.Lazy.Char8 (pack)
import qualified Data.ByteString as BS

{-
"product_sku","product_name","category_path","product_desc",
"file_url","file_url_thumb","file_title","file_description",
"file_meta","file_ordering"
-}
------------------- Data Types -----------------

data CSVProduct = CSVProduct {
	product_sku :: String,
	product_name :: String,
	category_path :: String, 
	product_desc :: String,
	file_url :: String,
	file_url_thumb :: String,
	file_title :: String

    } deriving (Show, Eq)

data VMProduct = VMProduct {
	sku :: String,
	name :: String,
	categories :: CategoryPaths,
	description:: String, 
	images :: [String],
	thumbnails :: [String]
}

data CategoryPaths = CPaths [[String]] 
-- We need a function that will rip apart all details from fields in record
-- like take apart '|' '/' to make a tree etc... 
parseProduct :: CSVProduct -> VMProduct
parseProduct prod@(CSVProduct s n p d u ut t) = 
	VMProduct s n cats d imgs thumbs 
	  where
	  	cats = parseCategoryTree p 
	  	imgs = splitEvery '|' u 
	  	thumbs = splitEvery '|' ut

splitEvery c = undefined
parseCategoryTree = undefined


-- How the whole type gets translated to into a record
instance ToNamedRecord VMProduct where
	toNamedRecord (VMProduct s n p d u ut) = 
		namedRecord ["product_sku"  .= s ,"product_name" .= n 
	                ,"category_path" .= p ,"product_desc" .= d 
	                ,"file_url" .= u ,"file_url_thumb" .= ut]

-- How values are written to a field 
-- maybe make CategoryPath a newtype
instance ToField a => ToField (Tree a) where
	toField (Node a []) = " "
	toField (Node x [Node a [], bs])  =  undefined

instance ToField CategoryPaths where
	toField (CPaths lss) =toField $ intercalate "|" $ map (intercalate "/") lss



instance ToField a => ToField [a] where
	toField = BS.concat . map (flip BS.append "|" . toField)

append = flip BS.append  	

instance FromNamedRecord CSVProduct where
	parseNamedRecord r = CSVProduct <$> r .: "product_sku"
	                                <*> r .: "product_name"
	                                <*> r .: "category_path"
	                                <*> r .: "product_desc"
	                                <*> r .: "file_url"
	                                <*> r .: "file_url_thumb"
	                                <*> r .: "file_title"

readFileNamed :: IO ()
readFileNamed = do
    putStr "Enter a file name: "
    name <- getLine
    csvData <- BL.readFile name
    case decodeByName csvData of
        Left err -> putStrLn err
        Right (h, v) -> do 
        	V.forM_ v $ \ p -> putStrLn (show (p :: CSVProduct)) 
        	putStrLn  $ "for header: " ++ (show h)	 



