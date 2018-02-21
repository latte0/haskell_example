{- ｀最大公約数が見つけられませんでした。 -}

import System.Environment
import Data.Ratio

data Rat = Rat { num::Integer, den::Integer }

minus :: Integer -> Integer -> Bool
minus a b = a < 0 && b < 0

myDiv :: Integer -> Integer -> Integer -> Integer
myDiv a b c= if abs a < abs (b) * c then c-1 else myDiv a b c+1

mc :: Rat -> Rat
mc a = Rat { num = if minus (num a) (den a) then -num a else num a , den = if minus (num a) (den a) then - den a else den a }

musd :: Rat -> Rat
musd a = Rat { num = if ((den a < 0) && (num a >= 0)) then -num a else num a , den = if ((den a < 0) && (num a >= 0)) then -den a else den a }

findbig :: Integer -> Integer -> Integer -> Integer -> Integer
findbig a b c d = if fromIntegral d >= fromIntegral a then c else (if a % d == 0 && b % d == 0 then (findbig a b d (d+10)) else (findbig a b c (d+10)))

ki :: Rat -> Rat
ki a = Rat { num = myDiv (num a) (findbig (abs(num a)) (abs(den a)) 1 1 ) 1 , den = myDiv (den a) (findbig (abs(num a)) (abs(den a)) 1 1) 1}

parse :: Rat -> Rat
parse a = ki $ musd $ mc a

instance Num Rat where
 x + y = parse $ Rat { num = num x * den y + num y * den x, den = den x * den y }
 x * y =  Rat { num = num x * num y, den = den x * den y }
 x - y =  Rat { num = num x * den y - num y * den x, den = den x * den y }
 negate x = x { num = - num x }
 abs x = Rat { num = abs (num x), den = abs (den x) }
 signum (Rat n d) | n == 0 = fromInteger 0
                  | n > 0 && d > 0 || n < 0 && d < 0 = fromInteger 1
                  | otherwise = fromInteger (-1)
 fromInteger x = Rat { num = x, den = 1 }


instance Show Rat where
 show x = show (num x) ++ "/" ++ show (den x)

instance Fractional Rat where
 x / y = Rat { num = num x * den y , den = den x * num y }
 fromRational x = Rat { num = numerator x, den =denominator x }

main = do
args <- getArgs
x <- return (read (args !! 0)::Integer)
y <- return (read (args !! 1)::Integer)
u <- return (read (args !! 2)::Integer)
v <- return (read (args !! 3)::Integer)
print $ Rat x y + Rat u v
print $ Rat x y - Rat u v
print $ Rat x y * Rat u v
print $ Rat x y / Rat u v
