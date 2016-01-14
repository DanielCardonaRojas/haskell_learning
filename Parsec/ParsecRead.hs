{- |
This module is a utility module for defining Read instances.

The general procedure for making X an instance of Read is to first make it an instance of ParsecRead and then doing: 

> instance X Read where readsPrec _ = parsecToReadS

Although the default instance (instance (ParsecRead a) => Read a where readsPrec _ = parsecToReadS) could be written to derive
instances automatically it involves language extensions (UndecidableInstances, FlexibleInstances) and some other trickery.

-}

module ParsecRead (ParsecRead (..),readInside,ParserR) where

import Text.Parsec

{- 
Recall Parsec Types:

type ReadS = String -> [(a,String)]

ParsecT s u m a is a parser that is paramatrized by a monad m, consumes a stream of type s and delivers some a.

type Parsec s u a = ParsecT s u Identity a
type Parser s a = Parsec s () a

parse p name input unwraps and runs the parser p on input. is an extra argument for debugging.

parse parsecRead' "" :: String -> Either ParseError [(a,String)]
-}

type ParserR a = Parsec String () a

class ParsecRead a where
    parsecToReadS :: String -> [(a, String)]
    parsecToReadS = eitherToList . parse parsecRead' "" 
    
    parsecRead :: ParserR a
	
eitherToList  = either (const []) id 

parsecRead' :: ParsecRead a => ParserR [(a,String)]
parsecRead' = do 
	a <- parsecRead
	rest <- getInput
	return [(a,rest)]


-- | A simple utility to read inside functors. 
readInside :: (Functor f, Read a) => f String -> f a 
readInside = fmap read 

-- Instances of Read Class 
-- This needs some king of Follow Undecidable instance Extension.

-- instance (ParsecRead a) => Read a where
-- 	readsPrec _ = parsecToReadS
