module Main where

import CsvUtils
import CartaTypes
import CartaStyles
import System.Environment (getArgs)
import Data.Either
import Lucid

{-
To load this in GHCI first do :set -XOverloadedStrings

Generate haddock Documentation runing this in terminal: 
-h is for Generating Html
-w turnoff warnings

haddock <fileName>.hs -o <folderNameForDocumentation> -h -w --optghc=-XOverloadedStrings
-}

-- Todo tidy up this function handle different args passing (e.g default outfileName)
main :: IO ()
main = do
	putStrLn "Usage: inputFile outputFile option [n/a]"
	[inF,outF, opt] <- getArgs
	x <- fmap rights $ readNamedRecords' inF :: IO [ItemCarta]
	let renderHtml = renderToFile (outF ++ ".html")
	case opt of 
		"2" -> renderHtml (styleItemRecords x)
	    "3" -> renderHtml (styleItemRecords3 x)
	
	


