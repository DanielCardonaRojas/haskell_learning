{-# LANGUAGE Arrows, NoMonomorphismRestriction #-}

import Text.XML.HXT.Core

data Guest = Guest {firstName, lastName :: String }
  deriving (Show, Eq)


doc = readDocument [withValidate no] "simple1.xml"
filt = multi (isElem >>> hasName "fname")
atTag tag = deep (isElem >>> hasName tag)

main = do 
  	res <- runX . xshow $ (doc >>> filt  >>> indentDoc)
  	mapM_ putStrLn res 
  	writeFile "out.xml" (unwords res)