module GetCmdOpts ( 
	renamingOpts,
	Flag (..)
	 ) where

import System.Environment ( getArgs )
import System.Console.GetOpt
import Data.Maybe( fromMaybe )
import Text.Read


renamingOpts = do
  args <- getArgs
  case getOpt RequireOrder options args of
    (flags, [],      [])     -> return flags
    (_,     nonOpts, [])     -> error $ "unrecognized arguments: " ++ unwords nonOpts
    (_,     _,       msgs)   -> error $ concat msgs ++ usageInfo header options

--Add an options to replace String by another String
data Flag =   TrimBeg Int  
			| TrimEnd Int 
			| Enumerate
			| EnumerateBeg
			| Delete String
			| EnumPrepending String 
			| EnumAppending String
            | EnumWith String
			| Clean 
			| Append String
			| Prepend String
			deriving (Show,Eq)

options :: [OptDescr Flag]
options = [
    Option ['c'] ["clean"] (NoArg Clean) "remove special characters, underscore and lowercase",
    Option ['a'] ["append"]   (ReqArg Append "String") "String that precedes to filename ",
    Option ['p'] ["prepend"]   (ReqArg Prepend "String") "String that procedes to filename ",
    Option ['d'] ["delete"]   (ReqArg Delete "String") "Ocurrence of string that get deleted",
    Option ['t'] ["trimEnd"]   (ReqArg (TrimEnd . read) "Number") "Number of chars that get dropped at the end",
    Option ['T'] ["trimBeg"]   (ReqArg (TrimBeg . read) "Number") "Number of chars that get dropped at the beginning",
    Option ['e'] ["enumPrep"]   (ReqArg EnumPrepending "String") "String that gets prepended to number",
    Option ['E'] ["enumApp"]   (ReqArg EnumAppending "String") "String that gets appended to number",
    Option ['n'] ["number"]   (NoArg Enumerate) "Enumerate file names at end",
    Option ['N'] ["numberBeg"]   (NoArg EnumerateBeg) "Enumerate file names at beginning",
    Option ['l'] ["enumWith"]   (ReqArg EnumWith "String") "All files will have this name plus an extra number at the end"

  ]
 

-- one possibility for handling optional file args:
-- if no file is provided as argument, read from stdin


header = "Usage: main [OPTION...]"