module CsvUtils (readRecords,readNamedRecords,printRecords,printNamedRecords) where

{-# LANGUAGE OverloadedStrings, ScopedTypeVariables #-}

import qualified Data.ByteString.Lazy as BL
import Data.Csv
import qualified Data.ByteString.Lazy.Char8 as CL
import qualified Data.Vector as V
import Data.Either



decodeByIndex1 :: FromRecord b => CL.ByteString -> Either String b
decodeByIndex1 = fmap (V.head) . decode NoHeader 

decodeByName1 :: FromNamedRecord b => CL.ByteString -> Either String (Header, b)
decodeByName1 = fmap (fmap V.head) . decodeByName

decodeAllByIndex :: FromRecord b => CL.ByteString -> [Either String b]
decodeAllByIndex = map decodeByIndex1 . CL.lines 

decodeAllByName:: FromNamedRecord b => CL.ByteString -> [Either String (Header, b)]
decodeAllByName bs = map decodeByName1 linesWithHeader  
					 where
						allLines = CL.lines bs 
						header = flip BL.append "\n" (head $ CL.lines bs) 
						linesWithHeader = map (BL.append header) (tail allLines)


readRecords :: FromRecord a => String -> IO [Either String a]
readRecords fname = do
    csvData <- BL.readFile fname
    return $ decodeAllByIndex csvData

readNamedRecords :: FromNamedRecord a => String -> IO [Either String (Header,a)]
readNamedRecords fname = do
    csvData <- BL.readFile fname
    return $ decodeAllByName csvData


printRecords m = do
			x <- fmap rights m
			mapM_ print x

printNamedRecords m = do 
	 x <- fmap (map snd . rights) m 
	 mapM_ print x 

