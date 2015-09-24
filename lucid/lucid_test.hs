{-#LANGUAGE OverloadedStrings, ExtendedDefaultRules #-}

import Lucid
import Control.Monad.Reader
import Data.Text (pack)

	{-
Notes: 
	Function signatures have to be written or else ambiguious types arrise.
	strings must be cast to html using (toHtml and tag atributes need to use pack or Text instead of string.)
-}

paragraphUsingName :: HtmlT (Reader String) ()
paragraphUsingName = do 
	name <- lift ask 
	if length name > 10 
	then p_ (toHtml name)
	else p_ $ strong_ "Daniel" 

--runReader (renderTextT paragraphUsingName) "hola"

example1:: Html ()
example1 = do 
	p_  (strong_ "")


example2::String -> Html ()
example2 t = do 
	p_  (span_ [style_ "color:red"] (toHtml t))
	p_ [class_ "si"]