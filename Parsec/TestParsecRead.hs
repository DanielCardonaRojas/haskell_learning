import ParsecRead
import Text.Parsec 


{- 
Remember to enable FlexibleInstances and UndecidableInstances
-}

main1 :: IO (Complex Int) 
main1 = putStrLn "Enter a complex numer I'll parse it: " >> readInside getLine

main :: IO Day
main = fmap (read) getLine

main2 :: FilePath -> IO ()
main2 filePath = do  
	res <- parseFile (many1 parserSRT) filePath 
	mapM_ print $ eitherToList res

---------------- Easy Example ---------------

data Day = Monday | Tuesday | Wednesday deriving (Show,Eq)
data Complex a = a :+: a deriving (Show,Eq)


instance ParsecRead Day where
	parsecRead =  (string "wed" >> return Wednesday) 
	         <|>  (string "mon" >> return Monday)
	         <|>  (string "tus" >> return Tuesday)


instance Read Day where
	readsPrec _ = parsecToReadS 

------------------- Complex Number Parsers --------
instance (Num a, Read a) => ParsecRead (Complex a) where
	parsecRead = parseComplex

instance (Num a, Read a) => Read (Complex a) where readsPrec _ = parsecToReadS 

instance Functor Complex where
	fmap f (a :+: b) = (f a) :+: (f b)

readNumber :: (Num a, Read a) => String -> a
readNumber = read 

sign :: Num a => ParserR (Complex a -> Complex a)
sign = do 
	s <- (char '-') <|> char '+'
	case s of
		'-' -> return $ toImag (* (-1))
		'+' -> return $ toImag (* 1)
--sign = undefined

toImag f (x :+: y) = x :+: f y	

parseComplex :: (Num a,Read a) => ParserR (Complex a)
--parseComplex :: ParserR (Complex String)
parseComplex = do 
        spaces
        real <- many1 digit
        spaces
        s <- sign  
        spaces
        imag <- many1 digit
        spaces
        char 'i'
        let (r :+: i) = fmap readNumber (real :+: imag) 
        return $ s (r :+: i)
------------------ Read srt file format --------------
data SRT = SRT SubtitleIndex TimeRange String deriving (Show)

type SubtitleIndex = Int
type TimeRange = (String,String)

parserSRT :: ParserR (SRT)
parserSRT = do 
	idx <- readInside $ many1 digit :: ParserR Int
	newline
	startTime <- many1 (digit <|> oneOf [':',','])
	spaces
	string "-->"
	spaces
	endTime <-  many1 (digit <|> oneOf [':',','])
	newline
	subtitle <- manyTill anyChar (try $ newline >> newline)
	return (SRT idx (startTime,endTime) subtitle)

instance ParsecRead SRT where
	parsecRead = parserSRT

instance Read SRT where readsPrec _ = parsecToReadS  
-------------------------- Utilities ----------------------

eitherToList = either (const []) id 

getFileExtension = reverse . takeWhile (/= '.') . reverse

parseFile :: ParserR a -> FilePath -> IO (Either ParseError a)
parseFile parser file = do 
	input <- readFile file
	let result = parse parser (getFileExtension file ++ " file format parser") input 
	return result

	