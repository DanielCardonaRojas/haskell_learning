import Data.Complex
import Data.List
import Control.Monad 

nthRoot n c = let 
				(r, theta) = polar c
				kthTheta k = (theta + 2 * pi * k) / n
				thetas = map (kthTheta) [0..n]
				r' = r ** (1/n)
				roots = map (mkPolar r') thetas
				in roots
				  

sqrt' = nthRoot 2
crt' = nthRoot 3
sixRoot' = sqrt' >=> crt'

---------------------------
-- Just as normal functions compose e.g sixthRoot = sqrt . crt
-- List monads enables multivalued functions to compose e.g sixthRoot' = sqrt' >=> crt'

(%) n m = let 
			division = n /m
			ratio = abs $ division
			sign = signum division
		  in case compare ratio 1 of
		  		LT ->  if sign > 0 then n else n + m
		  		GT ->  if sign > 0 then (%) (n - m) m else (%) (n + m) m
		  		EQ -> 0




