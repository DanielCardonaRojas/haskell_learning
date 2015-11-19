module TryParse (readRecords) where 


{-# LANGUAGE ScopedTypeVariables #-}

import qualified Data.ByteString.Lazy as BL
import Data.Csv
import qualified Data.ByteString as B 
import qualified Data.ByteString.Char8 as C


--Parses all records 
parseMap :: FromRecord a => C.ByteString -> [Either String a]
parseMap = fmap (runParser . parseRecord . record . getFields') . C.lines 

--TODO: Create an equivalent function for named records
-- parseMap' :: FromRecord a => C.ByteString -> [Either String a]
-- parseMap' ls = fmap (runParser . parseNamedRecord . namedRecord . getFields') $ zip (repeat header) rs 
-- 			where 
-- 				sl = C.lines ls
-- 				header = C.head sl
-- 				rs = C.tail ls 

getFields' = C.splitWith (== ',')


readRecords :: FromRecord a => String -> IO [Either String a]
readRecords fname = do
    csvData <- B.readFile fname
    return $ parseMap csvData
    