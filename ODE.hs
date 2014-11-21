module ODE where

import GOdeCalculator
import Grossone

	
--ODE
	--let fxy = (\x y -> x `gSub` y)
--initial condition
	--let x0 = gPair 0 0
	--let y0 = gPair 1 0

main = do
	let fxy = (\x y -> (gNegate (gPair 8 0)) `gMult` (gSub y (gPair (-20) 0)) )
	let x0 = gPair 0 0
	let y0 = gPair 100 0
	line <- getLine
	let k = read line :: Integer
--	putStrLn $ show $ gDeltaK k fxy x0 y0
	--putStrLn $ show $ gOdeComputeEulerFDs k fxy x0 y0
	putStrLn $ show $ gDelta0_K k fxy x0 y0--all deltk from o to k
	--putStrLn $ show $ gDerivatives0_K k fxy x0 y0--all deltk from o to k



{-main = do
	let a = foldr1 (gAdd) [(gPair 1 14) , (gPair (negate 2.8) 0),(gPair 3 (negate 0.3))]
	let b = foldr1 (gAdd) [(gPair 5 7.2), (gPair 2.4 (negate 3.1))]
	putStrLn $ "Num iterazioni: " 
	line <- getLine
	let k = read line :: Integer
	putStrLn $ "A= " ++ show a
	putStrLn $ "b= " ++ show b
	putStrLn $ "div = " ++  show (gDivide a b k)

-}

