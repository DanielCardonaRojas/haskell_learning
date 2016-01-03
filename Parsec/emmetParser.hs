{-# LANGUAGE OverloadedStrings, FlexibleContexts #-}
import Text.Parsec
-- import Text.Parsec.Text
import Control.Monad.Trans
import Control.Monad

import Lucid 
import Data.Text (pack,Text)
import Data.Tree
{-
The goal is to write a parser that will read an emmet 
string for constructing some html and parse it to html types.

From the lucid library.

Recall 

Parsec = ParsecT s u Identity a


-}
type Parser' a = Parsec a ()
type Parser = Parsec String ()


data Tag =  Div | P | Span deriving (Show, Eq)

type HtmlTag = (Tag,Attributes)

data HtmlTree = Tree HtmlTag 

type Attributes = [(Selector, Value)]

type Selector = String
type Value = String


-- runParser' :: Parser a -> String -> Either ParseError a
runParser' p text = parse p "An Emmet-Html parser" text

-------------- Parsing Helpers / Combinators -------------

enclosedBy :: Char -> Char -> Parser a -> Parser a
enclosedBy o c p = between (char o) (char c) p

-- tagP t = string t

-- Simple parse witch will be further transformed into more meaningful parsers. 
greaterThan :: Parser Char
greaterThan = char '>'

plusSign :: Parser Char
plusSign = char '+'

dot :: Parser Char
dot = char '.'

-------------- Actual Html parsers ---------------------

aWord :: Parser String 
aWord = many1 letter

aValue = many1 alphaNum

divTag :: Parser Tag
divTag = string "div" >> return Div 

classAtt = do
	dot 
	aWord

attr = between spaces spaces $ do 
	s <- aWord
	between spaces spaces (char '=')
	-- v <- enclosedBy '"' '"' anyChar
	v <- aValue
	return (s,v)

attributes = enclosedBy '[' ']' (many attr) 

tag = do
	t <- aWord
	manyTill classAtt (greaterThan <|> plusSign)


-------------------- Test 2 --------------------------------

type HParser = ParsecT String (Html ()) (Html)  

tagP :: HParser ()
tagP = do 
	x <- many1 alphaNum	
	char '>'
	v <- many1 alphaNum
	modifyState (mappend $ div_ (toHtml v))
	getState >>= lift 

runHParser p = runParserT p mempty "Emmet-Parser"  







