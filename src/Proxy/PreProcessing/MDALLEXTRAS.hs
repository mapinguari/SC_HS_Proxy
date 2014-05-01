module MDALLEXTRAS where

--Largest Rectangles Decomposition
maxRectDecomp :: [[Tile]] -> [Rectangle Int] 
maxRectDecomp = evalState allMax.tileToWalk
  where tileToWalk = map (map walkable)

allMax :: State [[Bool]] [Rectangle Int]
allMax = do 
  r <- nextMax 
  case r of
    Nothing -> return []
    Just r -> do 
      rs <- allMax
      return $ r:rs

nextMax :: State [[Bool]] (Maybe (Rectangle Int))
nextMax = state (\xs -> let h = length xs 
                            w = length.head $ xs in case mapMax xs of 
                          Just max -> (Just max, mask applyMask (craftMask h w max) xs)
                          otherwise -> (Nothing,xs))

mapMax :: [[Bool]] -> Maybe (Rectangle Int)
mapMax ys = maximum.catMaybes. map (uncurry maxRect2).assocs $ h
  where h = (array (1,(length ys)) $ [(i, zipWith g (h!(i-1)) ys)| (i,ys) <- tail $ zip nats (xs)] ++ [(1, head xs)])
        g x y = if y == 0 then 0 else x+y
        xs = map (map (\b -> if b then 1 else 0)) ys
        nats = iterate (+1) 1
        maximum xs = case xs of
          [] -> Nothing
          otherwise -> Just $ maximumBy largerOrSquarer xs   
        
maxRect2 :: Int -> [Int] -> Maybe (Rectangle Int)
maxRect2 n xs = if area a == 0 
                then Nothing 
                else Just $ addHeight n a
  where a = maxRectHist2 xs
        addHeight x r = Rectangle (xI r) (mkInterval n ((n-).sup.yI $ r))
                     

maxRectHist2 :: [Int] -> Rectangle Int
maxRectHist2 xs = mRHI2 (length xs) (zip xs (iterate (+1) 0)) [] (Rectangle (mkInterval 0 0) (mkInterval 0 0))

mRHI2 :: Int -> [(Height,Int)] -> [((Height,Height),Int)] -> Rectangle Int -> Rectangle Int
mRHI2 w ws@(~(new:xs)) zs@(~(((h,l),s):ys)) r
  | null ws && null zs = r
  | null zs = mRHI2 w xs [opening2 0 new] r
  | null ws = mRHI2 w [] ys (largestSquarest endOfLine r)
  | x == h = mRHI2 w xs zs r
  | x > h = mRHI2 w xs (trace2 (opening2 h new : zs)) r
  | x < h = mRHI2 w ws (trace2 surivingIncompleteRectangles) (largestSquarest largestClosed r) 
  where endOfLine = snd $ closing2 ((h,l),s) (0,w)
        x = fst new
        surivingIncompleteRectangles = case continue of  
          Nothing -> ys
          Just a -> a : ys
        (continue,largestClosed) = closing2 ((h,l),s) new

closing2 :: ((Height,Height) , Int) -> (Height , Int) -> (Maybe ((Height,Height),Int),Rectangle Int)
closing2 ((h,l),s) (nh,c) 
  | h == l = (Nothing, finalPiece h)
  | otherwise = (Just ((nh,l),s), finalPiece h)
  where finalPiece x = Rectangle (mkInterval s c) (mkInterval 0 x)
             
opening2 :: Height -> (Height,Int) -> ((Height,Height),Int)
opening2 h (nh,s) = if nh > h then ((nh, h+1),s) else ((h,h),s)

trace2 x = traceShow x x
------------Using Lists MAx REct------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------

maxRectHist :: [Int] -> Rectangle Int
maxRectHist xs = mRHI (length xs) (zip xs (iterate (+1) 0)) [] []
        
mRHI :: Int -> [(Height,Int)] -> [([Height],Int)] -> [Rectangle Int] -> Rectangle Int
mRHI w ws@(~(new:xs)) zs@(~((hs,s):ys)) rs
  | null ws && null zs = maximumBy largerOrSquarer rs
  | null zs = mRHI w xs [opening 0 new] rs 
  | null ws = mRHI w [] ys (endOfLine ++ rs) 
  | x == h = mRHI w xs zs rs
  | x > h = mRHI w xs (opening h new : zs) rs
  | x < h = mRHI w ws surivingIncompleteRectangles (ended ++ rs) 
  where h = head hs
        x = fst new
        endOfLine = snd $ closing (hs,s) (0,w)
        surivingIncompleteRectangles = if null.fst $ continue 
                                       then ys
                                       else continue : ys
        (continue,ended) = closing (hs,s) new


closing :: ([Height] , Int) -> (Height,Int) -> (([Height],Int),[Rectangle Int])
closing (xs,s) (h,c) = ((bs,s),map finalPiece as)
  where finalPiece x = Rectangle (mkInterval s c) (mkInterval 0 x)
        (as,bs) = span (> h) xs
        
opening :: Height -> (Height,Int) -> ([Height],Int)
opening h (nh,s) = ([nh, (nh -1) .. (h+1)],s)
-------------------------------------------------------------------------------------------------------------------------------------------------------------------
craftMask :: Height -> Width -> Rectangle Int -> [[Bool]]                        
craftMask h w r = [[f i j | i <- [0..(w-1)]]| j <- [0..(h-1)]]
                  where f i j = not $ mkPointxFirst (fromIntegral i + 0.5) (fromIntegral j + 0.5) `isInRect` r
                                     
applyMask :: [Bool] -> [Bool] -> [Bool]
applyMask = zipWith (&&)

mask :: (a -> b -> c) -> [a] -> [b] -> [c]
mask f mask ys 
  | length mask /= length ys = error "Mask not correct size"
  | otherwise = zipWith f mask ys

------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------

quickerMapDecomp :: [[Tile]] -> [Rectangle Int]
quickerMapDecomp ts = evalState (fastAllMax (height*width)) (tileToWalk ts)
  where tileToWalk = map (map walkable)
        height = length ts
        width = length.head $ ts

fastAllMax :: Int -> State [[Bool]] [Rectangle Int]
fastAllMax m = do 
  r <- nextMax3 m
  case r of 
    Nothing -> return []
    Just r -> if trace (show $ area r) (area r == 1)
              then do 
                map <- get
                return $ r : singles map
              else do 
                rs <- fastAllMax (area r)
                return $ r : rs

nextMax3 :: Int -> State [[Bool]] (Maybe (Rectangle Int))
nextMax3 m = state (\xs -> let h = length xs 
                               w = length.head $ xs in case mapMax3 m xs of 
                             Just max -> (Just max, mask applyMask (craftMask h w max) xs)
                             otherwise -> (Nothing,xs))

mapMax3 :: Int -> [[Bool]] -> Maybe (Rectangle Int)
mapMax3 m ys = maximum.catMaybes. map (uncurry (maxRect3 m)).assocs $ h
  where h = (array (1,(length ys)) $ [(i, zipWith g (h!(i-1)) ys)| (i,ys) <- tail $ zip nats (xs)] ++ [(1, head xs)])
        g x y = if y == 0 then 0 else x+y
        xs = map (map (\b -> if b then 1 else 0)) ys
        nats = iterate (+1) 1
        maximum xs = case xs of
          [] -> Nothing
          otherwise -> Just $ maximumBy largerOrSquarer xs   

maxRect3 :: Int -> Int -> [Int] -> Maybe (Rectangle Int)
maxRect3 m n xs = if area a == 0 
                then Nothing 
                else Just $ addHeight n a
  where a = maxRectHist3 m xs
        addHeight x r = Rectangle (xI r) (mkInterval n ((n-).sup.yI $ r))

maxRectHist3 :: Int -> [Int] -> Rectangle Int
maxRectHist3 m xs = mRHI3 m (length xs) (zip xs (iterate (+1) 0)) [] (Rectangle (mkInterval 0 0) (mkInterval 0 0))

--Add new condition basically checking the largest rectangle that currently open.
mRHI3 :: Int -> Int -> [(Height,Int)] -> [((Height,Height),Int)] -> Rectangle Int -> Rectangle Int
mRHI3 mArea w ws@(~(new:xs)) zs@(~(((h,l),s):ys)) r
  | null ws && null zs = r
  | null zs = mRHI3 mArea w xs [opening2 0 new] r
  | null ws = mRHI3 mArea w [] ys (largestSquarest endOfLine r)
  | x == h = mRHI3 mArea w xs zs r
  | x > h = mRHI3 mArea w xs (opening2 h new : zs) r
  | x < h = mRHI3 mArea w ws surivingIncompleteRectangles (largestSquarest largestClosed r) 
  where endOfLine = snd $ closing2 ((h,l),s) (0,w)
        x = fst new
        surivingIncompleteRectangles = case continue of  
          Nothing -> ys
          Just a -> a : ys
        (continue,largestClosed) = closing2 ((h,l),s) new
        
singles :: [[Bool]] ->  [Rectangle Int]
singles xs = [mkSingle i j | (row,j) <- zip xs nats, (b,i) <- zip row nats, b]
  where nats = iterate (+1) 0
        mkSingle i j = Rectangle (mkInterval i (i+1)) (mkInterval j (j+1))
















