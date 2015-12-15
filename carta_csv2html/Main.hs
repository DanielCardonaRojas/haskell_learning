module Main where

import CsvUtils
import CartaTypes
import CartaStyles
import System.Environment (getArgs)
import Data.Either
import Lucid
import Customizable

{-
To load this in GHCI first do :set -XOverloadedStrings

Generate haddock Documentation runing this in terminal: 
-h is for Generating Html
-w turnoff warnings

haddock <fileName>.hs -o <folderNameForDocumentation> -h -w --optghc=-XOverloadedStrings
-}

-- Todo tidy up this function handle different args passing (e.g default outfileName)
getFileName = reverse . dropUntil '.' . reverse where dropUntil c = tail . dropWhile (/= c)

main :: IO ()
main = do
	args <- getArgs
	case length args of 
		2 -> do
			let [inF,opt] = args 
			process inF (getFileName inF) opt
		3 -> do 
			let [inF,outF, opt] = args
			process inF outF opt
		_ -> putStrLn "Usage: inputFile outputFile option [n/a]"

process :: String -> String -> String -> IO ()
process inF outF opt = do 
	x <- fmap rights $ readNamedRecords' inF :: IO [ItemCarta]
	let renderHtml = renderToFile (outF ++ ".html")
	renderHtml (styleCartaItems (read opt ::Int) x)

process' :: Int -> String -> String -> String -> IO ()
process' s inF outF opt = 
	let 
	  renderHtml h = renderToFile (outF ++ ".html") (styleCartaItems (read opt :: Int) h)
	  correctParse = fmap rights $ readNamedRecords' inF :: IO [ItemCarta]
	in case s of 
	       0 -> correctParse >>= renderHtml 
	       1 -> correctParse >>= renderHtml . map (BrasasItemCarta) 
	       2 -> correctParse >>= renderHtml . map (Sushi7ItemCarta)






	    	
	    	
	    	


 
	  