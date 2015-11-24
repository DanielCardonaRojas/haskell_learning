module TryParse (readRecords,readNamedRecords) where 


{-# LANGUAGE ScopedTypeVariables #-}

import qualified Data.ByteString.Lazy as BL
import Data.Csv
import qualified Data.ByteString as B 
import qualified Data.ByteString.Char8 as C

{-
record :: [ByteString] -> Record
namedRecrod :: [(ByteString,ByteString)] -> NamedRecord
-}

--Parses all records 
parseAllRecords  :: FromRecord a => C.ByteString -> [Either String a]
parseAllRecords  = fmap (runParser . parseRecord . record . getFields) . C.lines 

parseAllNamedRecords :: FromNamedRecord a => C.ByteString -> [Either String a]
parseAllNamedRecords = fmap (runParser . parseNamedRecord) . (map namedRecord . getNamedRecords)

getRecords = fmap (record . getFields) . C.lines

getNamedRecords bs = map (zip (nameHeader bs)) $ listOfRecords bs 
          where             
             listOfRecords = tail . map getFields . C.lines  
             nameHeader = getFields . head . C.lines

getFields = C.splitWith (== ',')


readRecords :: FromRecord a => String -> IO [Either String a]
readRecords fname = do
    csvData <- B.readFile fname
    return $ parseAllRecords  csvData

readNamedRecords :: FromNamedRecord a => String -> IO [Either String a]
readNamedRecords fname = do 
	csvData <- B.readFile fname
	return $ parseAllNamedRecords csvData
    