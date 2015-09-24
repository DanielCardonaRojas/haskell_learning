import Text.XML.HXT.Core
import Text.HandsomeSoup


main :: IO ()
main = do
	let doc = fromUrl "http://en.wikipedia.org/wiki/Narwhal"
	links <- runX $ doc >>> css "#bodyContent a" ! "href"
	print links