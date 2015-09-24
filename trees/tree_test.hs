import Data.Tree
import Data.Char
import Text.Parsec
import Text.Parsec.String
import Control.Applicative ((<*),(*>), (<$>),(<*>),pure)
import Control.Monad
import Control.Monad.Trans (lift)
----------------- Menu of Website ------------
menu = 
	Node "main" [
	Node "Inicio" [],
	Node "Hidrualica" [
						Node "Movil" [
							Node "Bombas Hidraulicas" [],
							Node "Cilindros Hidraulicos" [],
							Node "Winches" [],
							Node "Valvulas" []
						],
						Node "Industrial" [
							Node "Valvulas" [],
							Node "Intercambiadores" [],
							Node "Unidades de Potencia" [],
							Node "Accesorios" []
						],
						Node "Agricola" [
							Node "Multiplicadores" [],
							Node "Toma de Fuerza" [],
							Node "Orbitrol" []
						],
						Node "Equipos" [
							Node "Unidades de Potencia" []
						]	
					  ],
	Node "Electricos" [
						Node "Motores y Ventiladores" [],
						Node "Moto Reductores" [],
						Node  "Variadores y Arracandores" [],
						Node "Maniobras" [],
						Node "Automatizacion" []

					  ],
	Node "Sellos" [],
	Node "Mangueras" [
						Node "Mangueras" [],
						Node "Conectores" [],
						Node "Racores" []

					 ]


				]
-------------------------------------

upperCase = map toUpper

-- putStrLn $ drawTree menu 
-- putStrLn $ drawTree (fmap upperCase menu)

----------- Parse and make Tree ----------
{-
A '.','-' or '*'' Represents indentation or nesting

so e.g: 

main
*Menu1
*Menu2
**Menu2.1
**Menu2.2
*Menu3

-}

dotMenu = ["-PMenu1","-SMenu2","--SSMenu2.1","--ssMenu2.2","-pMenu3"]

fromFileMenu = unlines dotMenu


----------- Parsing ---------

doParse rule text = parse rule "(source)" text

--runParser node () "menu" fromFileMenu 
indentation :: Parser Char
indentation = do
	 x <- char '*' <|> char '.' <|> char '-'
	 return x

valuelabel :: Parser String
valuelabel = do
	 x<- manyTill (letter <|> digit) newline 
	 return x

count' n p = do 
	x <- many p 
	let l = length x 
	if l == n then (return x) else (return [])


eofLine :: Parser ()
eofLine = char '\n' >> return ()



-- node value and indentation level
node :: Parser (String,Int)
node = do 
	c <- many1 indentation
	spaces 
	x <- valuelabel
	return (x, length c) 

-- runParser (many node' >> getState) 0 "" "---"
node' :: Parsec String Int ()
node' = do 
	x <- char '-'
	modifyState (+ 1) 
	return ()

nodes = do 
	many node'
	val <- many letter
	(Node val) <$> (forest)

forest' :: Parsec String Int (Tree String)
forest' = do 
	m <- getState
	n <- nodes >> getState
	if m == n then nodes else nodes

forest'' = many nodes




nodeWithLevel :: Int -> Parser (String)
nodeWithLevel n = do 
	count n indentation 	
	x <- valuelabel
	return (x)


equalNodes :: Int -> Parser ([String])   
equalNodes atLevel = do
		nodeLabels <- try (many (nodeWithLevel atLevel)) 
		return nodeLabels
	



--node :: Parsec String Int ()
--node = do 
--	c <- many1 indentation
--	spaces 
--	x <- manyTill anyChar indentation
--	return (x, length c) 









