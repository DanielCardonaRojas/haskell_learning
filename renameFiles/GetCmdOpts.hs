module GetCmdOpts ( 
    renamingOpts,
    Flag (..),
    FileSelector (..),
    makeReplace,
    getFileSelection,
    isFileSelection
     ) where

import System.Environment ( getArgs )
import System.Console.GetOpt
import Data.Maybe( fromMaybe )
import Text.Read
import Data.List

import FileName (getExt)
-- | renamingOpts retuns a list of parsed command line options for renaming files. i.e all possible operations 
-- the program supports
renamingOpts = do
  args <- getArgs
  case getOpt RequireOrder opciones args of
    (flags, [],      [])     -> return flags
    (_,     nonOpts, [])     -> error $ "unrecognized arguments: " ++ unwords nonOpts
    (_,     _,       msgs)   -> error $ concat msgs ++ usageInfo header' opciones

-- | Command line arguments are parsed into one of the following constructors.
data Flag =   TrimBeg Int  -- ^ Trim a number of char off starting from left to right
            | TrimEnd Int -- ^ Trim n char from the end 
            | ListUsing String -- ^ Enumerate files prepending a constant string
            | Enumerate -- ^ Enumerate files at the end
            | EnumerateBeg
            | Delete String -- ^ Delete occurrences of a substring
            | EnumPrepending String -- ^ Replace file names for a constant string and prepend an increasing number.
            | EnumAppending String -- ^ Replace file names for a constant string and appending an increasing number.
            | Clean -- ^ Clean up file name replace white space for _ remove sybolic chars, remove trailing spaces.
            | Append String -- ^ Append a string to each file name
            | Prepend String -- ^ Prepend a string to each file name
            | Replace String String -- ^ Replace all instances of 
            | FileSelection FileSelector -- ^ A selection to perform the operation on. 
            | GroupEnum -- ^ Files named similar get enumerated.
            deriving (Show,Eq)

-- | A type to specify a kind of selection
-- e.g files with certain extension or substring or just all files.
data FileSelector = WithExt String | WithSubstring String | OnAll deriving (Show, Eq)


isFileSelection :: Flag -> Bool 
isFileSelection (FileSelection _) = True
isFileSelection _ = False

getFileSelection :: [Flag] -> Maybe Flag
getFileSelection = find isFileSelection

makeFileSelector str 
    | not (null $ getExt str) = WithExt $ getExt str
    | null str = OnAll
    | otherwise = WithSubstring str  

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
    Option ['l'] ["listWith"]   (ReqArg ListUsing "String") "Enumerate files prepending a constant string",
    Option ['r'] ["replace"]   (ReqArg makeReplace "oldString|newString") "Replace old substring with new string",
    Option ['s'] ["selection"]   (ReqArg (FileSelection . makeFileSelector) "String") "Choose a file extension or file name substring",
    Option ['g'] ["groupEnum"] (NoArg GroupEnum) "Enumerates files by groups of similar names"
  ]
 
opciones :: [OptDescr Flag]
opciones = [
    Option ['c'] ["clean"] (NoArg Clean) "remueve characteres especiales, pone en minuscular y agregar guion bajo",
    Option ['a'] ["append"]   (ReqArg Append "String") "Anteceder palabra a todos los nombres",
    Option ['p'] ["prepend"]   (ReqArg Prepend "String") "Agregar palabra al final del los nombres",
    Option ['d'] ["delete"]   (ReqArg Delete "String") "Borrar todas apariciones de la palabra",
    Option ['t'] ["trimEnd"]   (ReqArg (TrimEnd . read) "Number") "Borrar el numero especificado de caracteres al final del nombre",
    Option ['T'] ["trimBeg"]   (ReqArg (TrimBeg . read) "Number") "Borrar el numero especificado de caracteres al principio del nombre",
    Option ['e'] ["enumPrep"]   (ReqArg EnumPrepending "String") "Enumerar precediendo una palabra",
    Option ['E'] ["enumApp"]   (ReqArg EnumAppending "String") "Enumerar posponiendo una palabra",
    Option ['n'] ["number"]   (NoArg Enumerate) "Enumerar nombre con numero al final del nombre",
    Option ['N'] ["numberBeg"]   (NoArg EnumerateBeg) "Enumerar nombre con numero al principio del nombre",
    Option ['l'] ["listWith"]   (ReqArg ListUsing "String") "Enumerar y remplazar nombre por palabra",
    Option ['r'] ["replace"]   (ReqArg makeReplace "oldString|newString") "Reemplazar palabra vieja por nueva",
    Option ['s'] ["seleccion"]   (ReqArg (FileSelection . makeFileSelector) "String") "Elige una extension o  sub nombre",
    Option ['g'] ["groupEnum"] (NoArg GroupEnum) "Enumerar por grupos, los archivos de nombres similares se enumeran separadamente"
  ]

-- one possibility for handling optional file args:
-- if no file is provided as argument, read from stdin

tupleSepBy cs = break' (flip elem cs)

break' p str = let (f,s) = break p str in (f, tail s)

makeReplace = twoArgs Replace
twoArgs f = uncurry f . (tupleSepBy "| ")

header = "Ways to use this program..."
header' = "Formas de uso de este programa..."


-- main = renamingOpts >>= mapM print