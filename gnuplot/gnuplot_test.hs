import Graphics.Gnuplot.Simple
import Graphics.Gnuplot.Terminal.X11
-- import Graphics.Gnuplot.Value.Tuple

main :: IO ()
main = test3


makelistVals f n = let x = [0..n] in zip x [f xo | xo <- x]


test2 = plotList [PNG "output1.png"] [0,5..100::Double]

test3 = plotListStyle [] (defaultStyle {lineSpec = CustomStyle [LineTitle "foobar"]}) [0,5..100::Double]

test0 = plotList [LineStyle 0 [LineTitle "foobar"]] $ makelistVals (sin :: Float -> Float) 100

test1 = plotFunc [terminal $ (title "hola" cons),YLabel "Sample value"] 
		  (linearScale 1000 (-20,20)) (\x -> (sin :: Float -> Float) x / x) 

