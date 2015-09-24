import Control.Monad.Trans.Writer
import Control.Monad.Trans
import Control.Monad

palindrome w = w' == w where w' = reverse w 

test1 :: WriterT [String] IO ()
test1 = do 
	x <- lift getLine
	if palindrome x then tell ([x]) else return ()

--runWriterT (replicateM_ 3 test1)