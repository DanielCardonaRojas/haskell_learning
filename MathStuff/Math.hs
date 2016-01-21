{-| 
Some math and function in the Style of Matlab
-}



linspace lower upper num = takeWhile (<= upper) $ iterate (+ inc) lower where inc = abs (upper - lower) / (fromInteger num)

evalInRange lower upper f num = map f $ linspace lower upper num

evalInRange' = uncurry evalInRange

