module TreeZipper 
   (
      Tree (..),
      navigateThroughPath,
      navigateDown,
      navigateToLeaf,
      navigateToParent

   )where 
   
import Data.Tree
import Data.Tree.Zipper
import Control.Applicative 
import Control.Monad 
import Data.List


productMenu  = 
	Node "Productos" [
     
            Node "Mujer" [
                  Node "Interior" [],
                  Node "Piyamas" []
            ],

            Node "Hombre" [
                  Node "Deportiva" [
                     Node "Camisas" [
                        Node "Sisa" [],
                        Node "Polo" []
                  ]],
                  Node "Casual" [ Node "traje" []],
                  Node "Invierno" []
            ],

            Node "Bebes" [
                  Node "Baberos" [],
                  Node "Biberon" []
            ]

	]

productZipper = fromTree productMenu
pz = productZipper

--------------- Printing Functions 
printFocus = printTree . tree
printTree = putStrLn . drawTree

-- label' :: (TreePos Full a) -> (Maybe a) 
label' = Just . label 

printForest :: Forest String -> IO ()
printForest = mapM_ printTree


-- Move to a node down the current focus  
moveHorizontalToNode :: Eq a => a -> TreePos Full a -> Maybe (TreePos Full a)
moveHorizontalToNode named tz = case label tz == named of 
						True -> Just tz
						False -> next tz >>= moveHorizontalToNode named 

navigateThroughPath :: Eq a => [a] -> TreePos Full a -> Maybe (TreePos Full a)
navigateThroughPath [] tz = Nothing
navigateThroughPath [x] tz = moveHorizontalToNode x tz
navigateThroughPath (x:xs) tz = 
      moveHorizontalToNode x tz >>= firstChild >>= navigateThroughPath xs
      

--search 2 levels deep 
navigate2Levels n tz = 
      moveHorizontalToNode n tz
      <|> (firstChild tz >>= moveHorizontalToNode n) 
      <|> (next tz >>= navigate2Levels n)


--Navigates to first leaf equal to l
navigateToLeaf :: Eq a => a -> TreePos Full a -> Maybe (TreePos Full a)
navigateToLeaf l tz | isLeaf tz && (label tz == l) = Just tz
navigateToLeaf l tz = 
      (firstChild tz >>= navigateToLeaf l)
      <|> (next tz >>= navigateToLeaf l) 

navigateToLeafInParent :: Eq a => a -> a -> TreePos Full a -> Maybe (TreePos Full a)
navigateToLeafInParent p l tz = navigateToParent p tz >>= navigateToLeaf l

-- Moves down the tree until it finds a parent labeled with p 
navigateToParent :: Eq a => a -> TreePos Full a -> Maybe (TreePos Full a)
navigateToParent p tz | hasChildren tz && (label tz == p) = Just tz
navigateToParent p tz = (firstChild tz >>= navigateToParent p) 
                        <|> (next tz >>= navigateToParent p)


{-navigates down a path of parent nodes until it 
reaches a leaf label with the last elem of the list 
a safe navigation version of navigateThruoghPath-}

navigateDownToLeaf :: Eq a => [a] -> TreePos Full a -> Maybe (TreePos Full a)
navigateDownToLeaf [] tz = Nothing
navigateDownToLeaf [x] tz = do
        m <- navigateToLeaf x tz 
        if isLeaf m then Just m else Nothing
navigateDown (x:xs) tz = navigateToParent x tz >>= navigateDownToLeaf xs                         


isDescendantOf n tz = (not . null) $ filter (== n) $ parentLabels (parents tz) 
              

parentLabels = map getParentLabel where getParentLabel (_,l,_) = l
--sumarize path

sumarizePath :: PosType t => TreePos t b -> [b]
sumarizePath tz = reverse $ parentLabels (parents tz) 

sumarizePathAsURL :: PosType t => TreePos t [Char] -> [Char]
sumarizePathAsURL = intercalate "/" . sumarizePath

