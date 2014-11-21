module GOdeCalculator where

import Grossone
import GOdeUtils


-- derivativeNumber -> ODE -> x0 -> y0
gOdeComputeEulerFDs ::Integer -> (Grossone -> Grossone -> Grossone) -> Grossone -> Grossone-> [Grossone]
gOdeComputeEulerFDs 0 _ _ y0 = [y0]
gOdeComputeEulerFDs k f x0 y0
	|k > 0 =  upToNow ++ [yi]
	|otherwise = error "ERROR : k has to be grater than 0"
	where 	
		upToNow = gOdeComputeEulerFDs (k-1) f x0 y0
		yi_1 = last upToNow
		yi = yi_1 `gAdd` ( gInfinitesimal `gMult`  fx_1y_1)
		fx_1y_1 = f (gMultScalar gInfinitesimal (fromInteger (k-1))) (yi_1)

gDeltaK :: Integer ->(Grossone -> Grossone -> Grossone)-> Grossone -> Grossone-> Grossone
gDeltaK k f x0 y0 = gDeltaKPrivate (gOdeComputeEulerFDs k f x0 y0) k k

--eulergrossoneFD ordered from y_0 to y_(length)
gDelta0_K :: Integer ->(Grossone -> Grossone -> Grossone)-> Grossone -> Grossone-> [Grossone]
gDelta0_K k f x0 y0 = map (\kk -> gDeltaKPrivate fe kk kk) [0..k]
	where fe = (gOdeComputeEulerFDs k f x0 y0)

 --eulergrossoneFD -> k -> currentiteration
gDeltaKPrivate :: [Grossone] -> Integer -> Integer-> Grossone
gDeltaKPrivate _ _ (-1) = []
gDeltaKPrivate fe k i
	| i `mod` 2 ==0 = gAdd val (gDeltaKPrivate fe k (i-1))
	|otherwise = gAdd (gNegate val) (gDeltaKPrivate fe k (i-1)) 
		where 
			val = gMultScalar (fe !! ((fromInteger k)-(fromInteger i)))  (fromInteger (binomialCoeff k i))
			

gDerivatives0_K :: Integer ->(Grossone -> Grossone -> Grossone)-> Grossone -> Grossone-> [(Integer, Double)]
gDerivatives0_K (-1) _ _ _ = []
gDerivatives0_K k f x0 y0 = [(k,gGetFirstGrossDigit (gGetPairPower ((gDelta0_K (k+1) f x0 y0) !!  (fromInteger (k-1))) (fromIntegral k)))] ++ gDerivatives0_K (k-1) f x0 y0







