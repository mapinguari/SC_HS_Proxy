module Proxy.Math.InfNumber where
import Control.Monad

data InfNumbers a = Inf | F a
                  deriving (Eq,Show)
                           
isInfinite :: InfNumbers a -> Bool
isInfinite Inf = True
isInfinite _ = False
               
getFinite :: InfNumbers a -> a
getFinite Inf = error "not finite"
getFinite (F a) = a
                           
instance (Num a) => Num (InfNumbers a) where 
  (+) = liftM2 (+)
  (-) = liftM2 (-)
  (*) = liftM2 (*)
  abs = liftM abs
  signum = liftM signum
  fromInteger = F . fromInteger
  
instance Monad InfNumbers where
  (>>=) Inf _ = Inf
  (>>=) (F a) f = f a 
  return a = F a
                           
instance (Ord a) => Ord (InfNumbers a) where
  compare Inf Inf = EQ
  compare Inf _ = GT 
  compare _ Inf = LT
  compare (F a) (F b) = compare a b  
  
instance (Real a) => Real (InfNumbers a) where
  toRational (F a) = toRational a
  toRational Inf = error "Infinity"