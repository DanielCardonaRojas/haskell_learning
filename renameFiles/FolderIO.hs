module FolderIO where

import System.Directory
import System.FilePath
import Control.Exception
import Data.List
import FileName
import ListModifiers


isFile :: FilePath -> Bool
isFile = hasExtension

isFolder:: FilePath -> Bool
isFolder = not . isFile

notDots p = p /= "." && p /= ".."
isHidden = (== '.') . head 
notHidden = not . isHidden

sandwich b1 b2 s = b1 ++ s ++ b2

------------- IO Functions ------------
writeListToTextFile :: FilePath -> String -> [String] -> IO ()
writeListToTextFile outputPath name folderList = do
	let listOfFolders = unlines folderList
	let outputFilePath = outputPath </> name
	writeFile outputFilePath listOfFolders

writeListOfFiles :: FilePath -> FilePath -> IO ()
writeListOfFiles output src = do 
	ff <- filesInFolders src
	let ff' = map (applyToFirst (sandwich "files_in_folder_" ".txt") ) ff
	mapM_ (uncurry (writeListToTextFile output)) ff'  

----------------- File IO Functions --------------------
getFileList :: FilePath -> IO [FilePath]
getFileList filePath = getDirectoryContents filePath >>= return . (filter (allOf [isFile,notHidden,nameIsNot "renameAllFiles",extIsNot "db"])) . sort
--getFileList filePath = getDirectoryContents filePath >>= (filterM doesFileExist)

getFileListFiltering :: (FilePath -> Bool) -> FilePath -> IO [FilePath]
getFileListFiltering p filePath = getDirectoryContents filePath >>= return . (filter (allOf [isFile,notHidden,nameIsNot "renameAllFiles",extIsNot "db", nameIs p]))

getFolderList :: FilePath -> IO [FilePath]
getFolderList filePath = getDirectoryContents filePath >>= return . (filter (allOf [isFolder, notHidden]))
--getFolderList filePath = getDirectoryContents filePath >>= filterM doesDirectoryExist

filesInFolders :: FilePath -> IO [(String,[FilePath])]
filesInFolders fp = do
	 folders <- getFolderList fp -- check if empty
	 let folders' = map (fp </>) folders 
	 files <- mapM getFileList folders'
	 return (zip folders files)

getCurrentFileList :: IO [FilePath]
getCurrentFileList = getCurrentDirectory >>= getFileList

isEmptyFolder :: FilePath -> IO Bool
isEmptyFolder src = fmap ((== 0) . length) (getFileList src) 

---------------------- Make copy of Files -------------------------
mkdir :: String -> FilePath -> IO FilePath
mkdir name src = do
	let filePath = src </> name 
	createDirectory filePath
	return filePath

mkdirE name src = 
	onException (putStrLn "An output folder already exists, enter a name for the new folder: " >> getLine>>= flip mkdir src) (mkdir name src)

mkdir' :: String -> FilePath -> IO FilePath
mkdir' name src = do
	let filePath = src </> name 
	exists <- doesDirectoryExist filePath
	isempty <- isEmptyFolder filePath
	if exists && isempty then return filePath else createDirectory filePath >> return filePath


copyFilesToFolder ::(String -> Bool) -> String -> FilePath -> FilePath -> IO FilePath
copyFilesToFolder p folderName src outDir = do
	files <- getFileListFiltering p src
	outFolder <- mkdirE folderName outDir
	let oldFilesWithPath = (map (src </>)) files
	let newFilesWithPath = (map (outFolder </>)) files
	let oldNew = zip oldFilesWithPath newFilesWithPath	
	mapM_ (uncurry copyFile) oldNew
	return outFolder

copyAllFilesToFolder = copyFilesToFolder (const True)

copyFilesIn p dir folderName = copyFilesToFolder p folderName dir dir

copyAllFilesIn :: FilePath -> String -> IO FilePath
copyAllFilesIn dir folderName = copyAllFilesToFolder folderName dir dir

