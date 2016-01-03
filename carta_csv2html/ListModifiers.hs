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

-- | Applies a function only to the head of a list
toHead f [] = []
toHead f (x:xs) = f x : xs 

toLast f ls = init ls ++ [f $ last ls]

mapInit f ls = (map f . init) ls ++ [last ls]


padAppending n d l = l ++ replicate n d

-- | 'padPreppending' n d l pads a list l prepending n d's.
padPrepending n d l = replicate n d ++ l

-- | 'splitEvery' n creates a list with sublists of length n.
splitEvery _ [] = []
splitEvery 0 ls = [ls]
splitEvery n list = first : (splitEvery n rest) 
    where (first,rest) = splitAt n list

-- | 'insertAt' n c is a function that insert elem c at index n 
insertAt n c l | length l < (n + 1) = l 
insertAt n c l = take (n+1) l ++ [c] ++ (drop (n+1) l)

-- | drop in an element every n elements
intersperseEvery n e = concat . intersperse [e] . splitEvery n

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


-- | Delete every occurrence of a sublist in a list
deleteWord :: Eq a => [a] -> [a] -> [a]
deleteWord = modifyWord (const []) 

deleteHeadWhile :: (t -> Bool) -> [t] -> [t]
deleteHeadWhile _ [] = []
deleteHeadWhile p (x:xs) = if p x then deleteHeadWhile p xs else (x:xs)

deleteLastWhile :: (t -> Bool) -> [t] -> [t]
deleteLastWhile _ [] = []
deleteLastWhile p xs = if p (last xs) then deleteLastWhile p (init xs) else xs


-- | Trim both ends of a list while a predicate holds for head and last element of a list
-- 
-- > deleteEndsWhile p = (deleteHeadWhile p) . (deleteLastWhile p)
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

-- | Collect all elements that are odd indices of a list 
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

-- | Compose a list of predicates ensuring every predicate is hold
--
-- > allOf = combinePredicates and
allOf :: [a -> Bool] -> a -> Bool
allOf = combinePredicates and

-- | At least one predicate in a list of predicates hold
--
-- > oneOf = combinePredicates or
oneOf :: [a -> Bool] -> a -> Bool
oneOf = combinePredicates or

-- | 'combinePredicates' logOp preds val, applies all preds to val yielding a list of bools 
-- witch are then combined with logOp 
combinePredicates :: ([Bool] -> Bool) -> [a -> Bool] -> a -> Bool
combinePredicates logicOp preds val = (logicOp . map ($ val)) preds




