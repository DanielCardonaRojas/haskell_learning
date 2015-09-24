module Main( main ) where

import System.Environment ( getArgs )
import System.Console.GetOpt
import Data.Maybe( fromMaybe )
import Text.Read


main = do
  args <- getArgs
  case getOpt RequireOrder options args of
    (flags, [],      [])     -> print (length flags) >> print flags
    (_,     nonOpts, [])     -> error $ "unrecognized arguments: " ++ unwords nonOpts
    (_,     _,       msgs)   -> error $ concat msgs ++ usageInfo header options

data Flag = Version | Input String | Output Int deriving (Show)

options :: [OptDescr Flag]
options = [
    Option ['V'] ["version"] (NoArg Version)            "show version number",
    Option ['k','i'] ["input"]   (ReqArg Input "FILE")      "some option that requires a file argument",
    Option ['o'] ["output"]  (ReqArg (Output . read) "FILE") "some option with an optional file argument"
  ]
 

-- one possibility for handling optional file args:
-- if no file is provided as argument, read from stdin
-- makeOutput :: Maybe String -> Flag
-- makeOutput ms = Output ( maybe "default" reverse ms)

header = "Usage: main [OPTION...]"