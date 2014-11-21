module Grossone where
import Data.List 


data GrossPair = Pair Double Double deriving (Eq)
			
instance Show GrossPair where
	show (Pair a b) = show a ++ "^"++ show b

instance Ord GrossPair where
	compare  (Pair a0 a1) (Pair b0 b1)
		| compPower /= EQ = compPower
		| otherwise = a0 `compare` b0
		where 
			compPower = a1 `compare` b1
			

type Grossone = [GrossPair]
--data Grossone = Grossone [GrossPair] deriving (Show)

--descending order
descending a b= compare b a


gGetFirstGrossDigit :: Grossone -> Double
gGetFirstGrossDigit ((Pair a _):[]) = a
gGetFirstGrossDigit ((Pair a _):xs) = a

gGetFirstGrossPower :: Grossone -> Double
gGetFirstGrossPower ((Pair _ b):[]) = b
gGetFirstGrossPower ((Pair _ b):xs) = b



gGetGrossDigit :: GrossPair -> Double
gGetGrossDigit (Pair a _) = a

gGetGrossPower :: GrossPair -> Double
gGetGrossPower (Pair _ b) = b

gInfinitesimal :: Grossone
gInfinitesimal = gPair 1 (negate 1)
 
gPair :: Double-> Double -> Grossone
gPair 0 _ = []
gPair d p = [Pair d p]

gGetPairPower :: Grossone -> Double-> Grossone
gGetPairPower [] _ = []
gGetPairPower (pa@(Pair ad ap):xs) p 
	|p==ap 		= [pa]
	|otherwise  = gGetPairPower xs p


--adding 
gAdd :: Grossone -> Grossone -> Grossone
gAdd a [] = sortBy (descending) a
gAdd [] b = sortBy (descending) b
gAdd (p@(Pair a0 a1):as) b@(b0:bs)	= gAdd as resA
	where resA = gAddPair p b

gAddPair :: GrossPair -> Grossone -> Grossone
gAddPair p [] = [p]
gAddPair p@(Pair p0 p1) b@(bp@(Pair b0 b1):bs) 
	| p1 == b1 = 
				if (b0+p0)/=0 
					then (Pair (b0+p0) p1):bs 
					else bs
	| otherwise = [bp] ++ gAddPair p bs
			

--multiply
--multiply
gMult :: Grossone -> Grossone -> Grossone
gMult a [] = []
gMult [] b = []
gMult a@(p:as) b	= sortBy (descending) $ gMultPrivate a b

gMultPrivate :: Grossone -> Grossone -> Grossone
gMultPrivate a [] = []
gMultPrivate [] b = []
gMultPrivate (p:as) b	= gAdd (gMultPair p b)  (gMultPrivate as b)

gMultScalar  :: Grossone -> Double -> Grossone
gMultScalar [] _ = []
gMultScalar _ 0 = []
gMultScalar ((Pair p0 p1):as) b	= [Pair (p0*b) p1] ++ gMultScalar as b


gMultPair :: GrossPair -> Grossone -> Grossone
gMultPair p [] = []
gMultPair (Pair 0 _) b = [] --never happens
gMultPair p@(Pair p0 p1) b@((Pair b0 b1):bs) = [Pair (p0*b0) (p1+b1)] ++ gMultPair p bs


--negate 
gNegate :: Grossone -> Grossone	
gNegate [] = []
gNegate ((Pair p0 p1):as) = [Pair (negate p0) p1] ++ gNegate as 

--substract
gSub :: Grossone -> Grossone -> Grossone
gSub a b = gAdd  a (gNegate b)

--power
gPow :: Grossone -> Int -> Grossone
gPow _ 0 = gPair 1 0 -- finite one
gPow [] _ = []
gPow a b = (gPow (gMult a a) (b-1))


--division witouth remainder
-- dividend -> dividend -> max iterations
gDivide :: Grossone -> Grossone ->Integer -> Grossone
gDivide a _ 0 =  a
gDivide a b k = gDividePrivate a b [] k

-- 				dividend ->    dividend -> result-> remainder -> maxiterations
gDividePrivate :: Grossone -> Grossone -> Grossone -> Integer -> Grossone
gDividePrivate _ _ c 0 = c
gDividePrivate a@((Pair ad ap):as) b@((Pair bd bp):bs) c  k 
	| null newR =  newC 
	| otherwise = gDividePrivate (newR) b (newC) (k-1)
		where
			clL = ad/bd
			lL = ap-bp
			newR = gSub as (tail (gMult b newPair))
			newC = gAdd c newPair
			newPair  = (gPair clL lL)
{-	

--division with remainder
-- dividend -> dividend -> maxiteration -
--gDivideRem :: Grossone -> Grossone ->Integer -> (Grossone,Grossone)

-}


	






