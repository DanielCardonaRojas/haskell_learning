{-# LANGUAGE ScopedTypeVariables, OverloadedStrings #-}

import qualified Data.ByteString.Lazy as BL
import Data.Csv
import qualified Data.Vector as V
import Data.Monoid
import Control.Monad

main0 :: IO ()
main0 = do
    csvData <- BL.readFile "test0.csv"
    case decode NoHeader csvData of
        Left err -> putStrLn err
        Right v -> V.forM_ v $ \(name, salary :: Int) ->
            putStrLn $ name ++ " earns " ++ show salary ++ " dollars"



-- Example 2

data Person = Person
    { name   :: !String
    , salary :: !Int
    }


-- Manual
instance FromNamedRecord Person where
    parseNamedRecord r = Person <$> r .: "name" <*> r .: "salary"

   


main1 :: IO ()
main1 = do
    csvData <- BL.readFile "test1.csv"
    case decodeByName csvData of
        Left err -> putStrLn err
        Right (_, v) -> V.forM_ v $ \ p ->
            putStrLn $ name p ++ " earns " ++ show (salary p) ++ " dollars"

-- Example 3

data MyTest = MyTest {first :: !String, rest :: [String]} deriving (Show, Eq)

instance FromRecord MyTest where
	parseRecord v = 
		let l = V.length v in 
		case  l > 2  of 
			True ->  MyTest <$> v .! 0 <*> (traverse (v .!) [1..2])
			False ->  mzero
	              

					     
    					             					  

												 	

main2 :: IO (V.Vector MyTest)
main2 = do
    csvData <- BL.readFile "test2.csv"
    case decode NoHeader csvData of
        Left err -> putStrLn err >> return V.empty
        Right v -> do 
        	V.forM_ v $ \ p -> putStrLn (show (p :: MyTest)) 
        	return v
        	
            











