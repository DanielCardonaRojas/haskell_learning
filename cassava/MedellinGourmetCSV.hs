{-# LANGUAGE ScopedTypeVariables, OverloadedStrings #-}

module MedellinGourmetCSV 
    (getCSVInfo,
     Info   
    ) where

import qualified Data.ByteString.Lazy as BL
import Data.Csv
import qualified Data.Vector as V


-- data Info = Info {
--                   restaurante :: String,    
--                   dir:: String, 
--                   tel:: String,
--                   cat :: String, 
--                   val :: String,
--                   restr:: String,
--                   nTab :: String, 
--                   web ::Maybe String,
--                   fb::Maybe String,
--                   twitter::Maybe String,
--                   instagram :: Maybe String,
--                   tripadvisor :: Maybe String,                                  
--                   entrada1:: String,
--                   platoFuerte1 :: String,
--                   postre1 :: String,
--                   postre2 :: String,
--                   entrada2:: String,
--                   platoFuerte2 :: String,
--                   postre3 :: String,
--                   entrada3:: String,
--                   platoFuerte3 :: String

--                  } deriving (Show)
    
type URL = String
data Infor = Infor {
                  restaurante :: String,   
                  dir::String, 
                  tel::String,
                  cat :: String, 
                  val :: String,
                  restr:: String,
                  nTab :: String, --Numero de mesas
                  web ::Maybe String,
                  fb::Maybe String,
                  twitter::Maybe String,
                  instagram :: Maybe String,
                  tripadvisor :: Maybe String,
                  datos :: [String]
                 } deriving (Show)

-- instance FromNamedRecord Info where
--     parseNamedRecord r = Info <$> r .: "Restaurante" <*> 
--     							  r .: "Direccion" <*> 
--     					          r .: "Telefono" <*>
--     					          r .: "Categoria" <*>
--     					          r .: "Valor Menu" <*> 
--     					          r .: "Restricciones" <*>
--     					          r .: "No. Mesas" <*> 
--     					          r .: "Sitio Web" <*>
--     					          r .: "Facebook" <*>
--     					          r .: "Twitter" <*>
--     					          r .: "Instagram" <*>
--     					          r .: "Tripadvisor"  <*>    					     
--     					          r .: "Entrada" <*>    					          
--     					          r .: "Plato Fuerte" <*>
--     					          r .: "Postre" <*>    					             					         					          
--     					          r .: "Entrada2" <*>
--     					          r .: "Plato Fuerte2" <*>
--     					          r .: "Postre2" <*>
--     					          r .: "Entrada3" <*>
--     					          r .: "Plato Fuerte3" <*>
--     					          r .: "Postre3"

instance FromRecord Infor where
    parseRecord v = Infor <$> v V.! 0 
                          <*>  v V.! 0 
                          <*>  v V.! 0 
                          <*>  v V.! 0 
                          <*>  v V.! 0 
                          <*>  v V.! 0 
                          <*>  v V.! 0 
                          <*>  v V.! 0 
                          <*>  v V.! 0 
                          <*>  v V.! 0 
                          <*>  v V.! 0 
                          <*>  v V.! 0 
                          <*> V.singleton [v V.! 0]


					     
    					             					  

												 	

-- main2 :: IO (V.Vector Info)
-- getCSVInfo = do
--     csvData <- BL.readFile "mgourmet.csv"
--     case decodeByName csvData of
--         Left err -> putStrLn err >> return V.empty
--         Right (_, v) -> do 
--         	V.forM_ v $ \ p -> putStrLn (show (p :: Infor)) 
--         	return v