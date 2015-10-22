module GetCmdOpts ( 
    renamingOpts,
    Flag (..),
    makeReplace
     ) where

import System.Environment ( getArgs )
import System.Console.GetOpt
import Data.Maybe( fromMaybe )
import Text.Read
import Data.List


renamingOpts = do
  args <- getArgs
  case getOpt RequireOrder opciones args of
    (flags, [],      [])     -> return flags
    (_,     nonOpts, [])     -> error $ "unrecognized arguments: " ++ unwords nonOpts
    (_,     _,       msgs)   -> error $ concat msgs ++ usageInfo header' opciones

--Add an options to replace String by another String
data Flag =   TrimBeg Int  
            | TrimEnd Int 
            | ListUsing String
            | Enumerate
            | EnumerateBeg
            | Delete String
            | EnumPrepending String 
            | EnumAppending String
            | Clean 
            | Append String
            | Prepend String
            | Replace String String
            | FileSelection String
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
    Option ['l'] ["listWith"]   (ReqArg ListUsing "String") "Enumerate files prepending a constant string",
    Option ['r'] ["replace"]   (ReqArg makeReplace "oldString|newString") "Replace old substring with new string",
    Option ['s'] ["selection"]   (ReqArg FileSelection "String") "Choose a file extension or file name substring"

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
    Option ['s'] ["seleccion"]   (ReqArg FileSelection "String") "Elige una extension o  sub nombre"
  ]

-- one possibility for handling optional file args:
-- if no file is provided as argument, read from stdin

tupleSepBy cs = break' (flip elem cs)

break' p str = let (f,s) = break p str in (f, tail s)

makeReplace = twoArgs Replace
twoArgs f = uncurry f . (tupleSepBy "| ")

header = "Ways to use this program..."
header' = "Formas de uso de este programa..."