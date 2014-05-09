{-


nodeToPosition :: Width -> Int -> Position
nodeToPosition w n = (q,r)
  where r = n `rem` w
        q = n `quot` w

------------------Total Potential of a Location  --------------------------

totalPotential :: PotCalc -> PotCalc -> PotCalc -> PotCalc -> PotCalc
totalPotential static dynamic objective historical = foldl1 (*) . (potentialFunctions <*>) . (flip (:)) []
  where potentialFunctions = [static,dynamic,objective,historical]

-----------------Basic Potential field stuff starts here ------------------
------sort out picking next location 
------sort out dangerous and mobile obstacle

--------------Locations selection and decision making----------------

---all locations occupied by a walk tile
tileToLocations :: Position -> [Location]
tileToLocations (x,y) = [(i,j) | i <- [(8*x)..(8*(x+1))], j <- [(8*y)..(8*(y+1))]]

---all locations occupied by unit
locationsOfUnit :: UnitData -> [Location]
locationsOfUnit uD = allLocations $ unitLocation uD
  where allLocations (z,w) = [(i,j) | i<-[z..z+(uW -1)], j<- [w..w+(uH -1)]]
        uH = (*8).tileHeight.unitType $ uD 
        uW = (*8).tileWidth.unitType $ uD

--all new totally new locations adjacent to current unit space
tilesToCheck :: UnitData -> [[Location]]
tilesToCheck uD = map allLocations corners
  where uH = (*8).tileHeight.unitType $ uD 
        uW = (*8).tileWidth.unitType $ uD
        (a,b) = unitLocation $ uD
        corners = [(i,j) | i <- [a..a+2*uW], j <- if i == a || i == a +2*uW
                                                  then [b..b+2*uH]
                                                  else [b,b+2*uH]]
        allLocations (z,w) = [(i,j) | i<-[z..z+(uW -1)], j<- [w..w+(uH -1)]]
        
-- list of locations adjacent to an arbitary rectangle given in height and width as number of locations
rectanglePerimeter :: Height -> Width -> Location -> [Location]
rectanglePerimeter h w (x,y) = [(i,j)| i<-[(x-1)..(x+h+1)], j <- if i == (x-1) || i == (x+w-1)
                                                                 then [(y-1)..(y+h+1)]
                                                                 else [(y-1),(y+h+1)]]
        
nullarySum :: (Num a,Ord a) => [a] -> a
nullarySum = foldr f 0 
  where f x y = if x < 0 then x else x + y

---------------Decision Making Potential ---------------
        
-- check all locations and find the one with the highest new potential
bestnewLocation :: UnitData -> Array Location Potential -> Maybe Location
bestnewLocation uD a = g . maximumBy f $ zip (map (nullarySum.(map (a!))) checkPositions) (map head checkPositions)
  where checkPositions = tilesToCheck uD
        f (n,p) (m,q) = compare n m 
        g (n,p) = if n == 0 then Nothing else Just p
        
-------------------Potential around units and static obstacles --------------

dangerousAir :: UnitData -> [(Location,Potential)]
dangerousAir uD = [(l,p j)| j <- [0..range] , l <- rectanglePerimeter (uH + j) (uW + j) (unitLocation uD) ]
  where range = airRange . unitType $ uD 
        p j = (fromIntegral j) * (1 / fromIntegral range)
        uH = (*8).tileHeight.unitType $ uD 
        uW = (*8).tileWidth.unitType $ uD

dangerousGround :: UnitData -> [(Location,Potential)]
dangerousGround uD = [(l,p j)| j <- [0..range] , l <- rectanglePerimeter (uH + j) (uW + j) (unitLocation uD) ]
  where range = groundMaxRange . unitType $ uD 
        p j = (fromIntegral j) * (1 / fromIntegral range)
        uH = (*8).tileHeight.unitType $ uD 
        uW = (*8).tileWidth.unitType $ uD
          
dynamicObstruction :: UnitData -> [(Location, Potential)]
dynamicObstruction uD = (concat [[(i,0.8)] |j <- [0..10], i <- (rectanglePerimeter (uH+j) (uW+j) (unitLocation uD))]) ++ staticObstruction uD
  where uH = (*8).tileHeight.unitType $ uD 
        uW = (*8).tileWidth.unitType $ uD

staticObstruction :: UnitData  -> [(Location,Potential)]
staticObstruction uD = zip (locationsOfUnit uD) (repeat 0)
        

staticMapPotential :: [[Tile]] -> [(Location,Potential)]
staticMapPotential xss = zip (concat $ map tileToLocations unwalkable) (repeat 0)
  where unwalkable = catMaybes . snd  $ mapAccumL f 0 (concat xss)
        f n t = if walkable t 
                then (n+1,Nothing)
                else (n+1,Just $ nodeToPosition (length.head $ xss) n)
        
---------------------------------------------
--------Simplest Example---------------------
        
airPotential :: Int -> Int -> [(Location,Potential)] -> Array Location Potential
airPotential h w xs = accumArray (*) 1 ((0,0),(8*h,8*w)) xs


airPotentialPairings :: [UnitData] -> [(Location,Potential)]
airPotentialPairings = concatMap f 
  where f uD 
          | canAttackAir.unitType $ uD = dangerousAir uD
          | isFlyer.unitType $ uD =  dynamicObstruction uD
          | otherwise = []


----------complete - O(n)--------------------
--swap the order of arguements for the append operation
airAndGroundPotential :: [[Tile]] -> [UnitData] -> (Array Location Potential,Array Location Potential)
airAndGroundPotential tss us = (accumArray (*) 1 ((0,0),(mH,mW)) affectsGround,
                                accumArray (*) 1 ((0,0),(mH,mW)) affectsAir)
  where mH = length tss
        mW = length.head $ tss
        affectsGround = staticMapPotential tss ++ gs
        affectsAir = as
        (gs,as) = foldl f ([],[]) us
        
        f (gs',as') uD = let uT = unitType uD in case (isFlyer uT, canMove uT, canAttackGround uT, canAttackGround uT) of
          (_,_,True,True) -> (dangerousGround uD ++ gs',dangerousAir uD ++ as')
          (False,False,False,False) -> (staticObstruction uD ++ gs', as')
          (False,True,False,False) -> (dynamicObstruction uD ++ gs',as')
          (False,_,False,True) -> (dangerousGround uD ++ gs',as')
          (False,False,True,False) -> (staticObstruction uD ++ gs', dangerousAir uD ++ as')
          (False,True,True,False) -> (dynamicObstruction uD ++ gs', dangerousAir uD ++ as')
          (True,_,False,False) -> (gs',dynamicObstruction uD ++ as')
          (True,_,False,True) -> (dangerousGround uD ++ gs', dynamicObstruction uD ++ as')
          (True,_,True,False) -> (gs',dangerousAir uD ++ as')
          
-}type Distance = Double
data Battlefield = Bat {bList :: [[Tile]],
                        bArray ::  Array Position Tile,
                        bGraph :: Gr Tile Int,
                        nodeToPos :: Node -> Position,
                        posToNode :: Position -> Node,
                        bHeight :: Int,
                        bWidth :: Int}


mkBat :: [[Tile]] -> Battlefield
mkBat xss = Bat {bList = xss,
                 bArray = bListToArray xss,
                 bGraph = bListToGraph xss,
                 nodeToPos = nodeToPosition w,
                 posToNode = positionToNode w,
                 bHeight = length xss,
                 bWidth = w}
  where w = length.head $ xss
                   
nodeToPosition :: Width -> Node -> Position
nodeToPosition w n = (q,r)
  where r = n `rem` w
        q = n `quot` w
        
        
positionToNode :: Width -> Position -> Node
positionToNode w (x,y) = x*w + y


bListToArray :: [[Tile]] -> Array Position Tile
bListToArray xs = array b (zip (range b) (concat xs))
  where b = ((1,1),(length xs,length.head $ xs))

bListToGraph :: [[Tile]] -> Gr Tile Int
bListToGraph xs = mkGraph a (dup (filter legalEdge adjacencies))
  where legalEdge (x,y,_) = not $ x `member` b || y `member` b
        adjacencies = [(i,j,1)| i <- range (0,c), j <- (adjacent i), j<i]
        dup = foldr g []
        g (x,y,z) zs = (x,y,z):(y,x,z):zs
        w = length.head $ xs
        h = length xs
        (a,b,c) = f (concat xs) 0 [] empty
        f [] n ys sT = (ys,sT,n-1)
        f (x:xs) n ys sT = f xs (n+1) ((n,x):ys) (if walkable x then sT else insert n sT)
        adjacent i
          | i == 0 = [i+1,w]
          | i == (w-1) = [i-1,i+w]
          | i == w*(h-1) = [i-w,i+1]
          | i == w*h - 1 = [i-w,i-1]
        --Corners
          | r == 0 = [i-1,i+w,i+1]
          | r == h-1 = [i-1,i-w,i+1]
          | c == 0 = [i-w,i+1,i+w]
          | c == w-1 = [i-w,i-1,i+w]
        --Edges
          |otherwise = [i-1,i-w,i+1,i+w]
          where (r,c) = nodeToPosition w i
            

battlefield n m = [[Tile 1 (f i j) False | j <- [0..(m-1)]]| i<- [0..(n-1)]]
                  where f i j = i /= j

printTile :: Tile -> Char
printTile t = case walkable t of
  True -> '+'
  False -> '-'

bfMap ::  (Graph gr) => gr Tile b -> [(Node,Int)] -> [(Node,Int)]
bfMap g xs | null xs || isEmpty g = []
bfMap g ((x,n):xs) = case match x g of 
  (Nothing,g') -> bfMap g' xs
  (Just (pre,_,_,_),g') -> (x,n) : bfMap g' (xs ++ (zip (map snd pre) (repeat (n+1))))
                                      
f :: (Graph gr,Show b) => gr Tile b -> Int -> [(Node,Int)]
f g m
  | m == 0 = []
  | length xs == 4 || length xs == 0 = f g (m-1)
  | otherwise = (n,1) : f g (m-1) 
  where (~(Just (_,n,_,xs),_)) = match m g
        
staticObstacleManhattan :: (Graph gr) => gr Tile () -> [(Node,Int)]
staticObstacleManhattan g = bfMap g xs
                       where xs = f g (noNodes g)

