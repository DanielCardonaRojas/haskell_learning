{-#LANGUAGE OverloadedStrings, ExtendedDefaultRules #-}

import Control.Monad
import Control.Monad.Reader
import Control.Monad.Trans.Maybe
import Lucid
import Data.Text (Text,pack)
import Data.Monoid 
import Control.Monad.Trans (lift)
import Data.Maybe( fromMaybe )
import Text.Read (readMaybe)

data GridConfig = 
	GridConfig {
	            numRows::Int, 
	            numCols::Int,
	            gutter::Maybe Int,
	            system :: GridSystem} 
	            deriving (Show,Eq)

data GridSystem = Bootstrap | Foundation deriving (Show,Eq,Read)

defConfig = GridConfig 2 3 Nothing Bootstrap
--readers
readRows :: Reader GridConfig Int
readRows = reader numRows

readCols :: Reader GridConfig Int
readCols = reader numCols

hasGutter :: Reader GridConfig (Maybe Int)
hasGutter = reader gutter

gridSys :: Reader GridConfig GridSystem
gridSys = reader system

colSize :: Reader GridConfig Int
colSize = do
	c <- readCols
	return (div 12 c)

--Grid System/Framework
mediumColClass :: Int -> Reader GridConfig Text 
mediumColClass n = do
	 sys <- gridSys
	 case sys of
	 	Bootstrap -> (return . pack) $ "col-sm-" ++ (show n) ++ " col-xs-12"
		Foundation -> (return . pack)  $ "medium-" ++ (show n) ++ " small-12 columns"

--row:: GridSystem -> Text
row _ = "row"

-- html blocks
mkRow :: HtmlT (Reader GridConfig) ()
mkRow = do
	r <- lift readCols
	s <- (lift colSize) 
	div_ [class_ "row"] $ replicateM_ r (mkColumn s)

mkColumn :: Int -> HtmlT (Reader GridConfig) ()
mkColumn n = do 
	c <- lift $ mediumColClass n 
	div_ [class_ c] ""
	return ()

mkGrid = do 
	rs <- lift readRows
	htmlReplicate rs mkRow

test = runReader (renderTextT mkGrid) defConfig

getHtml conf = runReader (renderTextT mkGrid) conf

htmlReplicate n = mconcat . replicate n 

-- IO get configuration 

askUser = \s -> do putStrLn s >> getLine
askUserForInt s = fmap readInt $ askUser s

readInt :: String -> Maybe Int 
readInt = readMaybe


getConfig :: MaybeT IO (GridConfig)
getConfig = do 
	cols <- MaybeT (askUserForInt "How many columns?")
	rows <- MaybeT (askUserForInt "How many rows?")
	gutter <- MaybeT (askUserForInt "How how much gutter?")
	framework <- MaybeT (fmap (readMaybe :: String -> Maybe GridSystem) $ askUser "Bootstrap or Foundation?")
	return (GridConfig rows cols (Just gutter) framework)


main = do 
	config <- runMaybeT getConfig
	let result = maybe "No parse" getHtml config
	return result
	
