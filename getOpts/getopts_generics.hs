{-# LANGUAGE DeriveGeneric #-}


import System.Console.GetOpt.Generics
import System.Environment
import qualified GHC.Generics

data Options
   = Options {
     number :: Int,
     string :: String
  }deriving (GHC.Generics.Generic)

instance Generic Options
instance HasDatatypeInfo Options 

main = do 
  (Options n s) <- getArguments
  doSomething n s

doSomething :: Int -> String -> IO ()
doSomething n s = print (n,s)  