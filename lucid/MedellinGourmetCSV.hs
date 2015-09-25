{-# LANGUAGE ScopedTypeVariables, OverloadedStrings #-}

module MedellinGourmetCSV 
    (
      getCSVInfo,
      Info (..)
    ) where

import qualified Data.ByteString.Lazy as BL
import Data.Csv
import qualified Data.Vector as V
import Data.Monoid
import Control.Monad


data Info = Info {
                  logo::String,
                  dir::String, 
                  tel::String,
                  cat:: String, 
                  val:: String,
                  restr:: String,
                  nTab :: String, --Numero de mesas
                  web ::Maybe String,
                  fb::Maybe String,
                  twitter::Maybe String,
                  instagram :: Maybe String,
                  tripadvisor :: Maybe String,
                  hasCoffee :: Bool,
                  datos :: [String]
                 } deriving (Show,Eq)


instance FromRecord Info where
    parseRecord r | V.length r > 15 = 
                         Info <$> r .! 0 
                              <*> r .! 1
                              <*> r .! 2
                              <*> r .! 3
                              <*> r .! 4
                              <*> r .! 5
                              <*> r .! 6
                              <*> r .! 7
                              <*> r .! 8
                              <*> r .! 9
                              <*> r .! 10
                              <*> r .! 11
                              <*> r .! 12
                              <*> (traverse (r .!) [13..15])
                    | otherwise = mzero          
					      					             					  

instance FromField Bool where
    parseField s
        | s == "si"  = pure True
        | s == "SI"  = pure True
        | s == "Si"  = pure True
        | otherwise = pure False						 	

getCSVInfo :: String -> IO (Maybe Info)
getCSVInfo fileName = do
    csvData <- BL.readFile fileName
    case decode NoHeader csvData of
        Left err -> putStrLn err >> return Nothing
        Right (v) -> do 
        	-- V.forM_ v $ \ p -> putStrLn (show (p :: Info)) 
        	return (v V.!? 1)




