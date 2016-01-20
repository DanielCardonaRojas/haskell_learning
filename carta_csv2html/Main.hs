{-#LANGUAGE OverloadedStrings, ScopedTypeVariables #-}
module Main where

import CsvUtils
import CartaTypes
import CartaStyles
import System.Environment (getArgs)
import Data.Either
import Lucid
import Customizable

{- |
To load this in GHCI first do :set -XOverloadedStrings

This Program depends on cassava for csv and lucid for html templating

install running: cabal install 

Compile with ghc --make Main.hs -XOverloadedStrings

Generate haddock Documentation runing this in terminal: 
-h is for Generating Html
-w turnoff warnings

haddock <fileName>.hs -o <folderNameForDocumentation> -h -w --optghc=-XOverloadedStrings

args example: "AnkaEntradas.csv" "0 a"
Use 0 for no wrapping in columns
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
		_ -> putStrLn "Usage: inputFile outputFile option [n/a]" >> putStrLn "option arg example: 1a"


process :: String -> String -> String -> IO ()
process inF outF opt = 
	let 
	  (cols, s) = parseOpt opt
	  renderHtml h = renderToFile (outF ++ ".html") (styleCartaItems cols h)
	  correctParse = fmap rights $ readNamedRecords' inF :: IO [ItemCarta]
	in case s of 
	       "" -> correctParse >>= renderHtml 
	       "b" -> correctParse >>= renderHtml . map (BrasasItemCarta) 
	       "s" -> correctParse >>= renderHtml . map (Sushi7ItemCarta)
	       "w" -> correctParse >>= renderHtml . map (WajacaItemCarta)
	       "v" -> correctParse >>= renderHtml . map (VillageItemCarta)
	       "a" -> correctParse >>= renderHtml . map (AnkItemCarta)
	       _  -> putStrLn "Couldn't match style option."


parseOpt :: String -> (Int, String)
parseOpt (c:cs) = (read [c], safeTail cs) where safeTail s = if null s then [] else tail s 




	    	
	    	
	    	


 
	  