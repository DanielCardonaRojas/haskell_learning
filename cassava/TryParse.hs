module TryParse (readRecords,readNamedRecords) where 


{-# LANGUAGE ScopedTypeVariables #-}

import qualified Data.ByteString.Lazy as BL
import Data.Csv
import qualified Data.ByteString as B 
import qualified Data.ByteString.Char8 as C
import qualified Data.Vector as V

{-
record :: [ByteString] -> Record
namedRecrod :: [(ByteString,ByteString)] -> NamedRecord
-}

--Parses all records 
parseAllRecords  :: FromRecord a => C.ByteString -> [Either String a]
parseAllRecords  = fmap (runParser . parseRecord ) . getRecords  

parseAllNamedRecords :: FromNamedRecord a => C.ByteString -> [Either String a]
parseAllNamedRecords = fmap (runParser . parseNamedRecord) . getNamedRecords

getRecords :: C.ByteString -> [Record]
getRecords = fmap (record . getFields) . C.lines

getNamedRecords :: C.ByteString -> [NamedRecord]
getNamedRecords bs = map namedRecord (addHeaderKeys $ listOfRecords bs) 
          where      
             addHeaderKeys = map (zip (nameHeader bs))       
             listOfRecords = tail . map getFields . C.lines  
             nameHeader = getFields . head . C.lines

getFields :: C.ByteString -> [C.ByteString]
getFields = C.splitWith (== ',')

getFields' :: (Functor f, ToField a) => f a -> f Field
getFields' = fmap toField 


readRecords :: FromRecord a => String -> IO [Either String a]
readRecords fname = do
    csvData <- B.readFile fname
    return $ parseAllRecords  csvData

readNamedRecords :: FromNamedRecord a => String -> IO [Either String a]
readNamedRecords fname = do 
	csvData <- B.readFile fname
	return $ parseAllNamedRecords csvData
    