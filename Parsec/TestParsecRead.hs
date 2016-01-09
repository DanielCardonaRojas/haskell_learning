import ParsecRead
import Text.Parsec 

{- 
Remember to enable FlexibleInstances and UndecidableInstances
-}

data Day = Monday | Tuesday | Wednesday deriving (Show,Eq)

instance ParsecRead Day where
	parsecRead =  (string "wed" >> return Wednesday) 
	         <|>  (string "mon" >> return Monday)
	         <|>  (string "tus" >> return Tuesday)

instance Read Day where
	readsPrec _ = parsecToReadS 

main :: IO Day
main = fmap (read) getLine