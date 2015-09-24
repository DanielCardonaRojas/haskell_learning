import Graphics.Google.Chart

main = do
	rawInput <- readFile "input.txt"
	rawInput2 <- readFile "input2.txt"
	let nums = map (read :: String -> Int) (lines rawInput)
	let nums2 = map (read :: String -> Int) (lines rawInput2)
	let l = length nums
	putStrLn $ chartURL $
		setSize 500 200 $
		setTitle "My test of Plotting a Chart in Haskell" $
		setData (encodeDataSimple [nums,nums2]) $
		setDataColors ["00ff00","ff0000"] $ 
		setLegend ["Stock Price","Sisas"] $
		setAxisTypes [AxisBottom,AxisLeft] $
		setAxisLabels (replicate 2 (map show [1..l])) $
		newLineChart