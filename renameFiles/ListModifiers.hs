module ListModifiers where

import Data.List

-------------------- Tuple Functions ----------------
swap (x,y) = (y,x)
applyToFirst f (x,y) = (f x,y)
applyToSecond f (x,y) = (x, f y)
applyToBoth f (x,y) = (f x, f y)
concatTuple (x,y) = x ++ y
concatTupleUsing sep (x,y) = x ++ sep ++ y

tuplify2 [x,y] = (x,y)
tuplify2With f g [x,y] = (f x, g y)
tuplify3 [x,y,z] = (x,y,z)
tuplify3With f g h [x,y,z] = (f x, g y, h z)

------------List Functions -----------

toHead f (x:xs) = f x : xs 

toLast f ls = init ls ++ [f $ last ls]

mapInit f ls = (map f . init) ls ++ [last ls]

-------------- Deleting and Modifying Lists Elems --------------
modifyWord :: Eq a => ([a] -> [a]) -> [a] -> [a] -> [a]
modifyWord _ _ [] = []
modifyWord f w str | length str < length w = str
				   | isPrefixOf w str = f w ++ modifyWord f w (drop (length w) str)
				   | otherwise = (head str) : modifyWord f w (tail str)

containsWord w str | length str < length w = False
				   | isPrefixOf w str = True
				   | otherwise = containsWord w (tail str)

replaceWord new old = modifyWord (const new) old

deleteWord :: Eq a => [a] -> [a] -> [a]
deleteWord = modifyWord (const []) 

deleteHeadWhile :: (t -> Bool) -> [t] -> [t]
deleteHeadWhile _ [] = []
deleteHeadWhile p (x:xs) = if p x then deleteHeadWhile p xs else (x:xs)

deleteLastWhile :: (t -> Bool) -> [t] -> [t]
deleteLastWhile _ [] = []
deleteLastWhile p xs = if p (last xs) then deleteLastWhile p (init xs) else xs

deleteEndsWhile :: (t -> Bool) -> [t] -> [t]
deleteEndsWhile p = (deleteHeadWhile p) . (deleteLastWhile p)

deleteExtra :: Eq a => [a] -> [a] -> [a]
deleteExtra ch str = concat $ map (headOrId $ allAreOneOf ch) $ groupBy (==) str
					 where allAreOneOf = all . (flip elem); headOrId p = (\group -> if p group then [head group] else id group)

--------------------- Predicates -----------------------

allOf :: [a -> Bool] -> a -> Bool
allOf = combinePredicates and

oneOf :: [a -> Bool] -> a -> Bool
oneOf = combinePredicates or

combinePredicates :: ([Bool] -> Bool) -> [a -> Bool] -> a -> Bool
combinePredicates logicOp preds val = (logicOp . map ($ val)) preds




