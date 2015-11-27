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
tuplify2 _ = error "tuplify2 must receive an input list of length 2"

tuplify2With f g [x,y] = (f x, g y)
tuplify3 [x,y,z] = (x,y,z)
tuplify3With f g h [x,y,z] = (f x, g y, h z)

uncurry3 :: (a -> b -> c -> d) -> (a,b,c) -> d
uncurry3 f = \(x,y,z) -> f x y z

curry3 :: ((a,b,c) -> d) -> (a -> b -> c -> d)
curry3 f = \x y z -> f (x,y,z)

------------List Functions -----------

toHead f (x:xs) = f x : xs 

toLast f ls = init ls ++ [f $ last ls]

mapInit f ls = (map f . init) ls ++ [last ls]

-- | 'splitEvery' n creates a list with sublists of length n.
splitEvery _ [] = []
splitEvery n list = first : (splitEvery n rest) 
    where (first,rest) = splitAt n list

-- | 'innerZip' takes a list and returns a a list of adjacent elements
-- If the input list has odd length then the last element is trimmed.
innerZip :: [a] -> [(a,a)]
innerZip = map tuplify2 . filter ((== 2) . length) . splitEvery 2  

innerZip3 :: [a] -> [(a,a,a)]
innerZip3 = map tuplify3 . filter ((== 3) . length) . splitEvery 3  


-------------- Deleting and Modifying Lists Elems --------------
-- | 'modifyWord' f w modifies every ocurrence of w with f
modifyWord :: Eq a => ([a] -> [a]) -> [a] -> [a] -> [a]
modifyWord _ _ [] = []
modifyWord f w str | length str < length w = str
				   | isPrefixOf w str = f w ++ modifyWord f w (drop (length w) str)
				   | otherwise = (head str) : modifyWord f w (tail str)

containsWord w str | length str < length w = False
				   | isPrefixOf w str = True
				   | otherwise = containsWord w (tail str)

-- | 'replaceWord' w nw replaces every occurence of w with nw
--
-- > replaceWord w nw = modifyWord (const nw) w
replaceWord old new = modifyWord (const new) old


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

-------------- decimate lists -------------
-- | decimate collects all even indecis of a list
decimate :: [a] -> [a]
decimate (x:_:xs) = x : decimate xs
decimate (x:_) = [x]
decimate _ = []

everyEven = decimate
everyOdd = decimate . tail

--------------------- Comparerisons -------------------
--sucesive equal elems in two lists
equalElems l1 l2 = (length . filter (== True) . takeWhile (== True)) $ zipWith (==) l1 l2

sharedElemsPerc l1 l2 = 
	let 
	  maxLen = fromIntegral $ max (length l1) (length l2)
	  eqs = fromIntegral $ equalElems l1 l2
	in 100 * (eqs / maxLen)

areSimilarBy perc l1 l2 = (>= perc) $ sharedElemsPerc l1 l2

groupSimilarBy p = groupBy (areSimilarBy p) . sort 

equalLengthSubLists ls =  map (take $ (minimum . map length) ls) ls 

zipEq l h =  takeWhile (/= ' ') $ (zipWith (\x y -> if x == y then x else ' ')) l h

withEqLen l = init $ scanl zipEq (head l) l
--------------------- Predicates -----------------------

allOf :: [a -> Bool] -> a -> Bool
allOf = combinePredicates and

oneOf :: [a -> Bool] -> a -> Bool
oneOf = combinePredicates or

combinePredicates :: ([Bool] -> Bool) -> [a -> Bool] -> a -> Bool
combinePredicates logicOp preds val = (logicOp . map ($ val)) preds




