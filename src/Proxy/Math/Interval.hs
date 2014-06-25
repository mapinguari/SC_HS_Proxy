module Proxy.Math.Interval (Interval,mkInterval, ilength, sup,inf,midpoint,trivial,isIn,intersection,trivialIntersection,interIntervalDistance,iUnion,iDist) where

newtype Interval a = I (a,a)
                   deriving (Show,Eq)
                            
mkInterval :: (Ord b) => b -> b -> Interval b
mkInterval x y = I (min x y, max x y)

ilength :: (Real a) => Interval a -> a
ilength (I (a,b)) = b - a

sup :: Interval a -> a
sup (I(_,b)) = b

inf :: Interval a -> a
inf (I(a,_)) = a

midpoint :: (Real a,Fractional b) => Interval a -> b
midpoint i = (/2).(realToFrac) $  inf i + sup i 

trivial :: (Eq a) => Interval a -> Bool
trivial i = sup i == inf i

isIn :: (Real a,Real b) => a -> Interval b -> Bool
isIn v i = toRational v <= toRational (sup i) && toRational v >= toRational (inf i)

intersection :: (Ord a) => Interval a -> Interval a -> Maybe (Interval a)
intersection a b = do
  i <- trivialIntersection a b
  if trivial i
    then Nothing 
    else Just i
                
trivialIntersection :: (Ord a) => Interval a -> Interval a -> Maybe (Interval a)
trivialIntersection (I (a,b)) (I (c,d)) 
  | d < a || b < c = Nothing
  | otherwise = Just $ I (max a c,min b d)
                
interIntervalDistance :: (Real a) => Interval a -> Interval a -> a
interIntervalDistance i1 i2 
  | ( sup i1 `isIn` i2) || ( inf i1 `isIn` i2 ) = 0
  | sup i1 < inf i2 = inf i2 - sup i1
  | sup i2 < inf i1 = inf i1 - sup i2
                      
iDist :: (Real a) => Interval a -> Interval a -> a
iDist = interIntervalDistance

iUnion :: (Real a) => Interval a -> Interval a -> Maybe (Interval a)
iUnion i1 i2 = case trivialIntersection i1 i2 of
  Just a -> Just ( I (min (inf i1) (inf i2), max (sup i1) (sup i2)))
  otherwise -> Nothing