{-# LANGUAGE ScopedTypeVariables, OverloadedStrings #-}

import Data.Traversable 
import Control.Applicative
import Data.Csv
import qualified Data.Vector as V
import qualified Data.ByteString.Lazy as BL
import Control.Monad
import Data.Monoid


data Producto = Producto {
					      productName:: !String,
                          productImages:: ![String]
                         } deriving (Show,Eq)
data Person = Person { name :: !String , salary :: !Int }


instance FromNamedRecord Producto where
    parseNamedRecord m = Producto <$> m .: "name"
                                  -- <*> traverse (m .:) ["","",""]
                                  <*> (sequence $ replicate 3 (m .: ""))

instance DefaultOrdered Person
    where headerOrder p = V.fromList ["name", "salary"]

-- Test 2 


data ProdInfo = ProdInfo String deriving (Show,Eq)
data ProdImages = ProdImages [String] deriving (Show, Eq)

data MyProduct = MyProduct ProdInfo ProdImages 

instance FromRecord ProdImages where
	-- parseRecord :: FromRecord a => Record -> Parser a
	parseRecord r = ProdImages <$> traverse (r .!) [1,2,3] 

instance FromNamedRecord ProdInfo where
	-- parseNamedRecord :: FromNamedRecord a => NamedRecord -> Parser a
	parseNamedRecord r = ProdInfo <$> (r .: "name") 



parseFileNamed :: String -> IO ()
parseFileNamed name = do
    csvData <- BL.readFile name
    case decodeByName csvData of
        Left err -> putStrLn err
        Right (h, v) -> do 
        	V.forM_ v $ \ p -> putStrLn (show (p :: Producto))
        	putStrLn  $ "for header: " ++ (show h)

main = putStr "enter a filename: " >> getLine >>= parseFileNamed