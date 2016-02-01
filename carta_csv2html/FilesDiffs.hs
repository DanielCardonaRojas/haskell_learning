import Data.List 

readFileLines = fmap words . readFile

nonInList baseList = find (not . flip elem baseList)

main = do 
	g <- readFileLines "archivoGrande.txt"
	s <- readFileLines "archivoPeque.txt"
	print $ nonInList g s 
	

