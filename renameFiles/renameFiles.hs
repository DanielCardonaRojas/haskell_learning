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

import Data.Foldable (find)

-- Custom Modules
import FolderIO
import FileName 
import ListModifiers
import GetCmdOpts 

trimFileEnd :: Int -> String -> String
trimFileEnd n s = modifyName (reverse . drop n . reverse) s

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

------------------------- Renaming Higher Order Functions ----------------------
renameFiles ::(String -> Bool) -> FilePath -> ([String] -> [String]) -> IO ()
renameFiles p src fun = do 
		oldFileList <- getFileListFiltering p src
		let newFileList = fun oldFileList
		let oldNewList = map (applyToBoth (src </>)) (zip oldFileList newFileList)
		let rename' = uncurry renameFile 
		mapM_ rename' oldNewList

renameAllFilesAt :: FilePath -> ([String] -> [String]) -> IO ()
renameAllFilesAt src fun =  renameFiles (const True) src fun

renameAllFilesUsing:: ([String] -> [String]) -> IO ()
renameAllFilesUsing fun = getCurrentDirectory >>= flip renameAllFilesAt fun

renameCopying:: (String -> Bool) -> String -> FilePath -> ([String] -> [String]) -> IO ()
renameCopying p folderName src fun = copyFilesIn p src folderName >>= flip renameAllFilesAt fun 

renameAllCopyingAt :: String -> FilePath -> ([String] -> [String]) -> IO ()
renameAllCopyingAt folderName src fun = copyAllFilesIn src folderName >>= flip renameAllFilesAt fun

renameAllCopiedFilesUsing :: ([String] -> [String]) -> IO ()
renameAllCopiedFilesUsing fun = getCurrentDirectory >>= flip (renameAllCopyingAt "outFolder") fun

renameFilesWithExtension :: (String -> Bool) -> ([String] ->[String]) -> IO ()
renameFilesWithExtension p modFun = getCurrentDirectory >>= \src -> renameFiles (extIs p) src modFun

----------------------------- MAIN PROCESSING FUNCTIONS  ----------------------------

performOnSelection :: FileSelector -> Flag -> IO ()
performOnSelection s o =
	let f = modifierFromOption o 
	in case s of 
		WithExt x -> renameFilesWithExtension (== x) f
		WithSubstring ss -> getCurrentDirectory >>= \src -> renameFiles (nameIs $ containsWord ss) src f
		OnAll -> renameAllFilesUsing f

 ----------- SELECTIN A MODIFYING FUNCTION TO APPLY ELEMENT WISE OR LIST WIE ----------
modifierFromOption = onListOrElementwise . selectOption

onListOrElementwise (OnList f) = f 
onListOrElementwise (Elementwise f) = map f

selectOption :: Flag -> SelectionModifier
selectOption Clean = Elementwise cleanUp
selectOption Enumerate = OnList enumEnd
selectOption EnumerateBeg = OnList enumBeg
selectOption (Append s) = Elementwise $ appendToFileName s
selectOption (Prepend s) = Elementwise $ prependToFileName s
selectOption (EnumPrepending s) = OnList $ enumPrepending s 
selectOption (EnumAppending s) = OnList $ enumAppending s 
selectOption (Delete s) = Elementwise $ (modifyName (deleteWord s)) 
selectOption (TrimEnd s) = Elementwise $ trimFileEnd s 
selectOption (TrimBeg s) = Elementwise $ trimFileBeg s
selectOption (ListUsing s) = OnList (enumEnd . (map $ modifyName (const s)))
selectOption (Replace o n) = Elementwise (replaceWord o n)
selectOption (FileSelection s) = OnList id 

data SelectionModifier = Elementwise (String->String) | OnList ([String]->[String])
data FileSelector = WithExt String | WithSubstring String | OnAll

fileSelectorFromArg :: Flag -> FileSelector
fileSelectorFromArg (FileSelection arg) = 
						  let 
						    isExtensionSel = not (null $ getExt arg) 
                          in 
                             if isExtensionSel 
                          	 then WithExt $ getExt arg
                          	 else WithSubstring arg 

isFileSelecion :: Flag -> Bool 
isFileSelecion (FileSelection s) = True
isFileSelecion _ = False

getFileSelection :: [Flag] -> Maybe Flag
getFileSelection = find isFileSelecion

main :: IO ()
main = do
	flags <- renamingOpts
	let fs =  maybe OnAll fileSelectorFromArg (getFileSelection flags)
	mapM_ (performOnSelection fs) flags 
	-- mapM_ performOption flags
	return ()







