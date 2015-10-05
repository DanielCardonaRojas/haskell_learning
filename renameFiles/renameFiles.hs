import System.Environment
import Control.Monad 
import Data.List
import Data.Char
import Control.Concurrent.Async
import System.FilePath
import System.Directory
import Control.Applicative
import Control.Monad.Trans (liftIO)
import Control.Monad.Writer 
import Data.Monoid
import Text.Read 
import Control.Exception

-- Custom Modules
import FolderIO
import FileName 
import ListModifiers
import GetCmdOpts 


trimFileEnd :: Int -> String -> String
trimFileEnd n = modifyName trimFromEnd
	where trimFromEnd = reverse . drop n . reverse 

trimFileBeg n = modifyName (drop n)

appendToFileName str = modifyName (++ str)
prependToFileName str = modifyName (str ++)

enumerateNames :: Bool -> Bool -> String -> [String] -> [String]
enumerateNames ascending atEnd sep names = 
			if atEnd 
			then zipWith appendToFileName seperatorNumber names
			else zipWith prependToFileName seperatorNumber' names
			where
				n = length names
				numbers = if ascending then [1..n] else take n $ iterate (+ (-1)) n
				seperatorNumber =  map ((sep ++).show) numbers
				seperatorNumber' = map ((++ sep).show) numbers

enumEnd = enumerateNames True True ""
enumBeg = enumerateNames True False ""

enumAndUnderscore = enumEnd . (map underscoreAndLower)
enumAndUnderscore' = enumBeg . (map underscoreAndLower)

enumPrepending str = enumEnd . map (modifyName (const str))
enumAppending str = enumBeg . map (modifyName (const str)) 

------------------------- Renaming Functions ----------------------
renameFiles ::(String -> Bool) -> FilePath -> ([String] -> [String]) -> IO ()
renameFiles p src fun = do 
		oldFileList <- getFileListFiltering p src
		let newFileList = fun oldFileList
		let oldNewList = map (applyToBoth (src </>)) (zip oldFileList newFileList)
		let rename' = uncurry renameFile 
		mapM_ rename' oldNewList

renameFiles' ::(String -> Bool) -> FilePath -> (String -> String) -> IO ()
renameFiles' p src fun = renameFiles p src (map fun)

renameAllFilesAt :: FilePath -> ([String] -> [String]) -> IO ()
renameAllFilesAt src fun =  renameFiles (const True) src fun

renameAllFilesAt' :: FilePath -> (String -> String) -> IO ()
renameAllFilesAt' src fun = renameAllFilesAt src $ map fun

renameAllFilesUsing:: ([String] -> [String]) -> IO ()
renameAllFilesUsing fun = getCurrentDirectory >>= flip renameAllFilesAt fun

renameAllFilesUsing':: (String -> String) -> IO ()
renameAllFilesUsing' fun = renameAllFilesUsing $ map fun

renameCopying:: (String -> Bool) -> String -> FilePath -> ([String] -> [String]) -> IO ()
renameCopying p folderName src fun = copyFilesIn p src folderName >>= flip renameAllFilesAt fun 

renameCopying' :: (String -> Bool) -> String -> FilePath -> (String -> String) -> IO ()
renameCopying' p folderName src fun = renameCopying p folderName src (map fun)

renameAllCopyingAt :: String -> FilePath -> ([String] -> [String]) -> IO ()
renameAllCopyingAt folderName src fun = copyAllFilesIn src folderName >>= flip renameAllFilesAt fun

renameAllCopyingAt' ::  String -> FilePath -> (String -> String) -> IO ()
renameAllCopyingAt' folderName src fun = renameAllCopyingAt folderName src $ map fun

renameAllCopiedFilesUsing :: ([String] -> [String]) -> IO ()
renameAllCopiedFilesUsing fun = getCurrentDirectory >>= flip (renameAllCopyingAt "outFolder") fun

renameAllCopiedFilesUsing' :: (String -> String) -> IO ()
renameAllCopiedFilesUsing' fun = renameAllCopiedFilesUsing $ map fun

------------------------------- MAIN ----------------------------

performOption :: Flag -> IO ()
performOption Clean = renameAllFilesUsing' cleanUp
performOption Enumerate = renameAllFilesUsing enumEnd
performOption EnumerateBeg = renameAllFilesUsing enumBeg
performOption (Append s) = renameAllFilesUsing' $ appendToFileName s
performOption (Prepend s) = renameAllFilesUsing' $ prependToFileName s
performOption (EnumPrepending s) = renameAllFilesUsing $ enumPrepending s 
performOption (EnumAppending s) = renameAllFilesUsing $ enumAppending s 
performOption (Delete s) = renameAllFilesUsing' $ (modifyName (deleteWord s)) 
performOption (TrimEnd s) = renameAllFilesUsing' $ trimFileEnd s 
performOption (TrimBeg s) = renameAllFilesUsing' $ trimFileBeg s
performOption (EnumWith s) = renameAllFilesUsing $ enumEnd . (map $ modifyName (const s))
performOption (Replace o n) = renameAllFilesUsing' (replaceWord o n)


main :: IO ()
main = do
	flags <- renamingOpts
	mapM_ performOption flags
	return ()







