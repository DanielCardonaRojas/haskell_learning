import ParsecRead
import Text.Parsec 

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

main2 :: FilePath -> IO ()
main2 filePath = do  
	res <- parseFile (many1 parserSRT) filePath 
	mapM_ print $ eitherToList res