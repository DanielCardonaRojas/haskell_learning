import Control.Monad
import Control.Monad.Trans.Maybe
import Control.Monad.Trans (lift)
import Data.Char

passOptions = ["coquitos","daniel"]


checkPass :: MaybeT IO ()
checkPass = do 
	x <- lift (putStrLn "Enter the password" >> getLine)
	guard (isValidPassword x) `mplus` (lift $ shouldExitOn x)
	continueWithPrompt

shouldExitOn s = iF' (== "exit") (putStrLn "Bye") (putStrLn "Try again") s

-- if p holds then apply f else apply g
iF :: (a -> Bool) -> (a -> b) -> (a -> b) -> a -> b 
iF p f g x = if p x then f x else g x  

iF' p tVal fVal = iF p (const tVal) (const fVal) 

-- isValidPassword = all isAlphaNum 
isValidPassword = flip elem passOptions

continueWithPrompt = return () 




