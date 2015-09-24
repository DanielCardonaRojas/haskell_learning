
-- Corecursion

evenN :: Int -> Bool
evenN 2 = True
evenN n = oddN (n - 1)

oddN :: Int -> Bool
oddN 0 = False
oddN 1 = True
oddN n = evenN (n-1)
