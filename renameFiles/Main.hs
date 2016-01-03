module Main where 

import System.Environment
import Control.Monad 
import Data.List 
import Data.Char
import System.FilePath
import System.Directory
import Control.Applicative
import Data.Monoid
import Data.Maybe (fromMaybe)

-- Custom Modules
import FolderIO
import FileName 
import ListModifiers
import GetCmdOpts 

{-
Update documentation running: 

haddock Main.hs -o docs -h -w
-}

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

-- | This function will enumerate simlarly named files by a give percent. Still needs some polishing.
enumSimilarBy p = concat . enumEnd1 . map equalLengthSubLists . groupSimilarBy p
                  where
                     enumEnd1 = \x -> if length x == 1 then x else map enumEnd x

------------------------- Renaming Higher Order Functions ----------------------
-- | 'renameFiles' p path f renames all files satisfying p at path with f.
renameFiles ::(String -> Bool) -> FilePath -> ([String] -> [String]) -> IO ()
renameFiles p src fun = do 
		oldFileList <- getFileListFiltering p src
		let newFileList = fun oldFileList
		let oldNewList = map (applyToBoth (src </>)) (zip oldFileList newFileList)
		let rename' = uncurry renameFile 
		mapM_ rename' oldNewList

-- | 'renameAlFilesAt' __p__ renames all files at p transforming the list of files names.
renameAllFilesAt :: FilePath -> ([String] -> [String]) -> IO ()
renameAllFilesAt src fun =  renameFiles (const True) src fun

-- | Renames all current files with a function that will transform the list of files into a 
-- new list of file names.
renameAllFilesUsing :: ([String] -> [String]) -> IO ()
renameAllFilesUsing fun = getCurrentDirectory >>= flip renameAllFilesAt fun

-- | renameCopying __pred folder path f__ renames files located at path satisfying pred with a function f
-- and puts them in a folder named folder. Leaving the original unchanged  
renameCopying :: (String -> Bool) -> String -> FilePath -> ([String] -> [String]) -> IO ()
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

-- | SelectionModifier wraps functions that operate on each file name in isolation or 
-- functions that depend on the context of other file names, such as order, amount of files etc.

data SelectionModifier = Elementwise (String->String) | OnList ([String]->[String])

modifierFromOption = onListOrElementwise . selectOption

onListOrElementwise (OnList f) = f 
onListOrElementwise (Elementwise f) = map f


selectOption :: Flag -> SelectionModifier
selectOption flg = 
        case flg of
         Clean -> Elementwise cleanUp 
         Enumerate -> OnList enumEnd
         EnumerateBeg -> OnList enumBeg
         (Append s) -> Elementwise $ appendToFileName s
         (Prepend s) -> Elementwise $ prependToFileName s
         (EnumPrepending s) -> OnList $ enumPrepending s 
         (EnumAppending s) -> OnList $ enumAppending s 
         (Delete s) -> Elementwise $ (modifyName (deleteWord s)) 
         (TrimEnd s) -> Elementwise $ trimFileEnd s 
         (TrimBeg s) -> Elementwise $ trimFileBeg s
         (ListUsing s) -> OnList (enumEnd . (map $ modifyName (const s)))
         (Replace o n) -> Elementwise (replaceWord o n)
         (GroupEnum) -> OnList (enumSimilarBy 20)
         (FileSelection _) -> error "FileSelection is not an action"


main :: IO ()
main = do
	flags <- renamingOpts
	let (FileSelection fs) =  fromMaybe (FileSelection OnAll) (getFileSelection flags)
	let flags' = filter (not . isFileSelection) flags -- remove the FileSelection flag since its not an operation.
	mapM_ (performOnSelection fs) flags' 
	return ()








