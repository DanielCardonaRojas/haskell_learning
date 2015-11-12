module TryParse (readRecords) where 


{-# LANGUAGE ScopedTypeVariables #-}

import qualified Data.ByteString.Lazy as BL
import Data.Csv
import qualified Data.ByteString as B 
import qualified Data.ByteString.Char8 as C


--Parses all records 
parseMap :: FromRecord a => C.ByteString -> [Either String a]
parseMap = fmap (runParser . parseRecord . record . getFields') . C.lines 

getFields' = C.splitWith (== ',')


readRecords :: FromRecord a => IO [Either String a]
readRecords = do
    csvData <- B.readFile "salaries.csv"
    return $ parseMap csvData
    