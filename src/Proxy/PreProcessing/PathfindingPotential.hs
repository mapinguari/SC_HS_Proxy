type Distance = Double
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

