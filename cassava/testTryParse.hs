import TryParse
import Data.Csv
import Control.Monad
import System.Environment
import Data.Either (rights)

--Try switching the data type for ident  field
data Person = Person {name :: !String, ident :: !DefIdent} deriving (Show, Eq)

-- Data type for custom field Reading
newtype DefIdent = DefIdent Int deriving (Eq, Show) 

instance FromRecord Person where
	parseRecord r | length r == 2 = Person <$> (r .! 0) <*> (r .! 1)
	              | otherwise = mzero

instance FromField DefIdent where
	parseField s = case runParser (parseField s) of
						Left err -> pure $ DefIdent (0)
						Right n -> pure $ DefIdent n 

onlySuccesfulParses :: FromRecord b => String -> IO [b]
onlySuccesfulParses = fmap rights . readRecords 

main = do 
    [fileName] <- getArgs
    rs <- readRecords fileName :: IO [Either String Person]
    let rs' = rights rs 
    let recordCount = length rs
    let succeeds = length rs'
    putStrLn $ "Parsed " ++ (show succeeds) ++ " out of " ++ (show recordCount) ++ " records"
    mapM_ print rs'