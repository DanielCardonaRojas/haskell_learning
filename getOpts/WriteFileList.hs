import System.Environment 
import System.Directory
import Options.Applicative
{- 
Este programa depende del packete optparse-applicative.
-}
getExt = reverse . takeWhile (/= '.') . reverse

isFile = not . null . getExt

isFileWithExt e = (== e) . getExt

getDir = getCurrentDirectory >>= getDirectoryContents

getFiles = filter isFile <$> getDir

getFilesWithExtension e = filter (isFileWithExt e) <$> getDir 


data FileOption = ExtesionSelector String | AllFiles

fileOption :: Parser FileOption 
fileOption = 
	ExtesionSelector 
	  <$> strOption (long "ext" <> short 'e' <> metavar "EXTENSION" <> help "Write files with some extension") 
	  <|> (pure AllFiles)

writeFileWithExtension :: FileOption -> IO ()
writeFileWithExtension (ExtesionSelector ext) = do
	filesString <- fmap unlines $ getFilesWithExtension ext
	writeFile "ListOfFiles.txt" filesString

writeFileWithExtension AllFiles = do 
		filesString <- fmap unlines getFiles 
		writeFile "ListOfFiles.txt" filesString


main = execParser opts >>= writeFileWithExtension
        where
		  opts = info (helper <*> fileOption)
		     (fullDesc 
		     	<> progDesc "Listar todos los archivos del directorio con un filtro para elegir la extension" 
		     	<> header "ListarArchivos - Daniel Cardona Rojas")
