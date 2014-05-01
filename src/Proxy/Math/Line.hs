module Proxy.Math.Line (Point,x,y,mkPoint,mkPointxFirst,mkPointyFirst,distance,Line,xCo,yCo,cCo,mkLine,invarientX,invarientY,xVal,yVal, lineIntersect, onLine,LineSeg,lineEq,start,end,lineSegLength,mkLineSegment,lineSegIntersect,onLineSeg) where 
import Proxy.Math.Interval
import GHC.Real


----------------Point -------------------------
data Point a = P {x,y :: a}
                  deriving (Show,Eq)

mkPoint :: a -> a -> Point a
mkPoint = mkPointxFirst 

mkPointxFirst :: a -> a -> Point a
mkPointxFirst r s = P {x = r,
                       y = s}

mkPointyFirst :: a -> a -> Point a
mkPointyFirst r s = mkPointxFirst s r

distance :: (Real a,Real b,Floating c) => Point a -> Point b -> c
distance p q = sqrt.fromRational $ ((toRational.x $ p) - (toRational.x $ q)) ^ 2 + ((toRational.y $ p) - (toRational.y $ q)) ^ 2

add :: (Num a) => Point a -> Point a -> Point a
add p1 p2 = mkPoint (x p1 + x p2) (y p1 + y p2)

negatePoint :: (Num a) => Point a -> Point a
negatePoint p = mkPoint (negate.x $ p) (negate.y $ p)

subtract :: (Num a) => Point a -> Point a -> Point a
subtract p1 = add p1.negatePoint

multiply :: (Num a) => a -> Point a -> Point a
multiply mu p = mkPoint ((mu*).x $ p) ((mu*).y $ p)
------------------------- Line ---------------------------

data Line = L {xCo,yCo,cCo :: Int}
            deriving (Show)

mkLine :: (Real a) => Point a -> Point a -> Line
mkLine p1 p2
  | a == z && b == w = error "None distinct points"
  | a == z = L ad 0 an
  | b == w = L 0 bd bn
  | otherwise = L xCoefficient yCoefficient cons
  where xCoefficient = px `div` g
        yCoefficient = py `div` g
        cons = pc `div` g
        g = gcd (gcd px py) pc
        px = ((wn*bd - wd*bn)*(zd*ad))
        py = ((an*zd - ad*zn)*(wd*bd))
        pc = (zn*bn*ad*wd - an*wn*zd*bd)
        a@(an :% ad) = ratioMap (fromInteger).toRational $ x p1
        b@(bn :% bd) = ratioMap (fromInteger).toRational $ y p1
        z@(zn :% zd) = ratioMap (fromInteger).toRational $ x p2
        w@(wn :% wd) = ratioMap (fromInteger).toRational $ y p2
        ratioMap :: (a -> b) -> Ratio a -> Ratio b
        ratioMap f (a :% b) = (f a :% f b)

invarientX :: Line -> Bool
invarientX = (==0).xCo 

invarientY :: Line -> Bool
invarientY = (==0).yCo

xVal :: (Fractional a) => a -> Line -> a
xVal a l 
  | xCo l == 0 = error "coefficient of zero"
  | otherwise = negate ((num yCo l) * a + (num cCo l) / (num xCo l))
    where num n = fromIntegral.n
          
yVal :: (Fractional a) => a -> Line -> a 
yVal a l 
  | yCo l == 0 = error "coefficient of zero"
  | otherwise = negate ((num xCo l) * a + (num cCo l) / (num yCo l))
    where num n = fromIntegral.n

instance Eq Line where 
  (==) (L a b c) (L x y z) = a == x && b == y && c == z

lineIntersect :: Line -> Line -> Maybe (Either (Point Rational) Line)
lineIntersect l1@(L a b h) l2@(L c d f) 
  | l1 == l2 = Just (Right l1)
  | det == 0 = Nothing
  | otherwise = Just (Left (mkPointxFirst (fromIntegral (h*d - b*f) / det) (fromIntegral (a*f - c*h) / det)))
  where det = fromIntegral $ a*d-b*c

onLine :: (Num a,Eq a) => Point a -> Line -> Bool 
onLine p (L a b c) = a'*(x p) + b'*(y p) + c' == 0
  where  a' = fromIntegral a
         b' = fromIntegral b
         c' = fromIntegral c
           
--------------------------LineSeg----------------------
          
data LineSeg = LS {lineEq :: Line, start, end :: Point Rational}
             deriving (Show)

instance Eq LineSeg where
  (==) ls1 ls2 = lineEq ls1 == lineEq ls2 && ((start ls1 == start ls2 && end ls1 == end ls2) || (end ls1 == start ls2 && start ls1 == end ls2))

mkLineSegment :: (Real a) => Point a -> Point a -> LineSeg
mkLineSegment a b = LS {lineEq = mkLine a b,
                        start = mkPoint (toRational.x $ a) (toRational.y $ a),
                        end = mkPoint (toRational.x $ b) (toRational.y $ b)}
                    
lineSegLength :: (Floating a) => LineSeg -> a
lineSegLength ls = distance (start ls) (end ls)

midPoint :: LineSeg -> Point Rational
midPoint ls = multiply (0.5) (add (start ls) (end ls))
                    
onLineSeg :: Point Rational -> LineSeg -> Bool
onLineSeg p ls
  | onLine p (lineEq ls) = if (x p) `isIn` (interval x) && (y p) `isIn` (interval y) then True else False
  | otherwise = False
  where interval n = mkInterval (n.start $ ls) (n.end $ ls)
        
lineSegIntersect :: LineSeg -> LineSeg -> Maybe (Either (Point Rational) LineSeg)
lineSegIntersect ls1 ls2 = case lineIntersect (lineEq ls1) (lineEq ls2) of
  Nothing -> Nothing
  Just (Left p) -> if onLineSeg p ls1 && onLineSeg p ls2 then Just (Left p) else Nothing
  Just (Right l) -> if invarientY l 
                    then f x mkPointxFirst (flip yVal l)
                    else f y mkPointyFirst (flip xVal l)
    where f n pointmk mVal = let p1 = n.start $ ls1
                                 p2 = n.end $ ls1
                                 p3 = n.start $ ls2
                                 p4 = n.end $ ls2
                                 i1 = mkInterval p1 p2
                                 i2 = mkInterval p3 p4 in case intersection i1 i2 of
                               Nothing -> Nothing
                               Just i -> Just (Right (LS l (pointmk (inf i) (mVal.inf $ i)) (pointmk (sup i) (mVal.sup $ i))))
        

        
  