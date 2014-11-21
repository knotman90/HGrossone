module GOdeUtils where

fact :: Integer -> Integer
fact 0 = 1
fact n = n* fact (n-1)


binomialCoeff :: Integer -> Integer -> Integer
binomialCoeff n k 
	|k < 0 = 0
	|k> n  = 0
	|otherwise =  (fact n) `div` ((fact k)*(fact (n-k)))
