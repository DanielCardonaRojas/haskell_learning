module FileName where

import Data.Char
import ListModifiers

----- File Name Functions  System.FilePath ----------
-- | returns a (name, ext) of file name
nameAndExt filename = let (name,ext) = break (== '.') filename in (name, dropWhile (== '.') ext)
getName = fst . nameAndExt
getExt = snd . nameAndExt

-- | modifyName f applies f to the filename excluding the extension
modifyName f = concatTupleUsing "." . applyToFirst (applySafe f) . nameAndExt
-- | modifyExt f applies f to the extension of a file name.
modifyExt f = concatTupleUsing "." . applyToSecond (applySafe f) . nameAndExt

-- | 'applySafe' f ls applies f to ls only if it doesnt emply an empty list as a result.
applySafe f x = let y = f x 
				in case y of 
					[] -> x
					_ -> y

nameWithExt (name,ext) = name ++ "." ++ ext

unwordsWithCommas = unwords . (mapInit (++ ","))

nameIs :: ([Char] -> Bool) -> [Char] -> Bool
nameIs p = p . getName

nameIsNot p = nameIs (/= p)

extIs :: ([Char] -> Bool) -> [Char] -> Bool
extIs p = p . getExt

extIsNot p = extIs (/= p)

------------- Utilities -------------
isElemOf :: Eq a => [a] -> a -> Bool
isElemOf = flip elem

isSymbolic = isElemOf "@#$%^&;:=+*|?><!'"

nameToLower = modifyName $ map toLower

nameToUpper = modifyName $ map toUpper

replaceIf p newCh = map (\c -> if p c then newCh else c)
replaceChar ch newChar = replaceIf (== ch) newChar
replaceSpacesBy ch = replaceIf isSpace ch

capitalize = unwords . (map cap) . words where cap = toHead toUpper

deleteSpace = filter $ not . isSpace

underscoreAndLower = (replaceChar '-' '_') . replaceSpacesBy '_' . nameToLower . modifyName (deleteEndsWhile isPunctuation)

camelCase = toHead toLower . deleteSpace . capitalize . (replaceChar '-' ' ') . (replaceChar '_' ' ')

concatWith sep str str2 =str ++ sep ++ str2 


cleanUpFileName = modifyName $ deleteEndsWhile (oneOf [isPunctuation,isSpace]) . deleteExtra ['_','-',' '] . filter (not . isSymbolic) . replaceChar 'ñ' 'n'
cleanUp = underscoreAndLower . cleanUpFileName







