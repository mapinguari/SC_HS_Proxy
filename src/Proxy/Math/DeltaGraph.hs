{-# LANGUAGE NoImplicitPrelude #-}
-- | Functional delta graphs
-- | Inductive
module Data.Graph.Delta.Inductive
where
import Control.Applicative ((<|>))
import Control.Parallel.Strategies (parMap, rdeepseq)
import Data.Foldable (toList, foldl, foldl')
import Data.Functor ((<$>))
import Data.List (find)
import Data.Maybe (isJust, maybeToList)
import Data.Sequence ((|>), (<|), (><), ViewL(..), viewl, ViewR(..), viewr, fromList, Seq, partition, singleton)
import qualified Data.Sequence as Seq (empty, null, length)
import Prelude hiding (pi, foldl)
--------------------
-- Abstract syntax
--------------------
-- | The graph definition
data Gamma = Gamma N V Pi Upsilon Delta deriving (Show, Eq)
-- | A list of deltas
type Delta = Seq D
-- | A delta is one of
data D = PlusND ND -- ^ Node insert
       | MinusN N -- ^ Node delete
       | PlusED ED -- ^ Edge insert
       | MinusE E -- ^ Edge delete
       | PlusGamma Gamma -- ^ Graph insert
       | UpGamma Gamma -- ^ Graph upsert
       deriving (Show, Eq)
-- | Inductive edges
type Pi = [(E, V, Gamma)]
-- | Cross edges
type Upsilon = [(E, V, N)]
-- | Node definition
type ND = (N, V, N, E, V)
-- | Edge definition
type ED = (E, V, N, N)
-- | Node key
type N = Int
-- | Edge key
type E = Int
-- | Edge and node value
type V = Int
-- | The graph type
type Graph = Gamma
-- | The empty gamma

empty :: Graph
empty = Gamma (-1) (-1) [] [] Seq.empty

atom :: Graph 
atom = Gamma 1 1 [] [] Seq.empty
--------------------
-- Additions,
-- removals,
-- uses
--------------------
-- | The node, if any, defined in a delta
addn :: D -> [N]
addn (PlusND (n, _, _, _, _)) = [n]
addn _ = []
-- | The edge, if any, defined in a delta
adde :: D -> [E]
adde (PlusND (_, _, _, e, _)) = [e]
adde (PlusED (e, _, _, _)) = [e]
adde _ = []
-- | The node, if any, removed in a delta
rmvn :: D -> [N]
rmvn (MinusN n) = [n]
rmvn _ = []
-- | The edge, if any, removed in a delta
rmve :: D -> [N]
rmve (MinusE e) = [e]
rmve _ = []
-- | The nodes, if any, used in a delta
usen :: D -> [N]
usen (PlusND (_, _, n', _, _)) = [n']
usen (PlusED (_, _, n, n')) = [n, n']
usen _ = []
--------------------
-- Incremental head/tail update
--------------------
-- | Update a delta to the tail of a gamma
gammaApplyDeltaT :: Gamma -> D -> Gamma
gammaApplyDeltaT (Gamma n v pi upsilon delta) d = Gamma n v pi upsilon (norm (delta |> d))
-- | Update deltas to the tail of a gamma
gammaApplyDeltasT :: Gamma -> Delta -> Gamma
gammaApplyDeltasT (Gamma n v pi upsilon delta) ds = Gamma n v pi upsilon (norm (delta >< ds))
-- | Update deltas to the head of a gamma
gammaApplyDeltasH :: Gamma -> Delta -> Gamma
gammaApplyDeltasH (Gamma n v pi upsilon delta) ds = Gamma n v pi upsilon (norm (ds >< delta))
-- | Update a pi to the tail of a gamma
gammaApplyPiT :: Gamma -> Pi -> Gamma
gammaApplyPiT (Gamma n v pi upsilon delta) pi' = Gamma n v (pi ++ pi') upsilon delta
-- | Update a delta and a pi to the tail of a gamma
gammaApplyDeltaPiT :: Gamma -> D -> Pi -> Gamma
gammaApplyDeltaPiT g d pi = gammaApplyDeltaT (gammaApplyPiT g pi) d
-- | Update a delta to the tail of a gamma indexed by an edge into pi
piApplyDeltaET :: Pi -> D -> E -> Pi
piApplyDeltaET [] _ _ = []
piApplyDeltaET ((e,v,g):ps) d e' | e == e' = (e, v, gammaApplyDeltaT g d) : ps
                                 | otherwise = (e, v, g) : piApplyDeltaET ps d e
-- | Update a delta to the tail of a gamma into every edge of a pi
piApplyDeltaStarT :: Pi -> D -> Pi
piApplyDeltaStarT pi d = map dapply pi
  where dapply (e, v, g) = (e, v, gammaApplyDeltaT g d)
-- | Update deltas to the tail of a gamma into every edge of a pi
piApplyDeltasStarT :: Pi -> Delta -> Pi
piApplyDeltasStarT = foldl piApplyDeltaStarT
-- | Propagate deltas through pi
prop :: Graph -> Graph
prop g@(Gamma n v pi upsilon d) = case viewl d of
  a :< d' -> Gamma n v (piApplyDeltaStarT pi a) upsilon d'
  EmptyL -> g
-- | Propagate specific delta
propD :: Graph -> D -> Graph
propD (Gamma n v pi upsilon d) d' = Gamma n v (piApplyDeltaStarT pi d') upsilon d
-- | Propagation condition
propCond :: D -> N -> Pi -> Upsilon -> Bool
propCond (MinusE e) _ pi upsilon = not (hasEdge e pi) || not (hasEdge e upsilon)
propCond (MinusN n') n _ upsilon = n /= n' && not (hasNodeU n upsilon)
propCond (PlusED (e, _, n', n'')) n pi upsilon = n /= n' && n /= n'' && propCond (MinusE e) n pi upsilon
propCond (PlusND (n', _, n'', e, _)) n pi upsilon = n' /= n && n' /= n'' && propCond (MinusE e) n pi upsilon
propCond (PlusGamma _) _ _ _ = False
-- | Find an edge in a pi or upsilon
findEdgePU :: E -> [(E, a, b)] -> Maybe (E, a, b)
findEdgePU e = find ((== e) . fst')
-- | Checks for the existence of an edge in a pi or upsilon
hasEdge :: E -> [(E, a, b)] -> Bool
hasEdge e = isJust . findEdgePU e
-- | Find a node in an upsilon
findNodeU :: N -> Upsilon -> Maybe (E, V, N)
findNodeU n = find ((== n) . thrd)
-- | Checks for the existence of a node in an upsilon
hasNodeU :: N -> Upsilon -> Bool
hasNodeU n = isJust . findNodeU n
-- | Find a node in a pi
findNodeP :: N -> Pi -> Maybe (E, V, Gamma)
findNodeP n = find ((== n) . (\(Gamma n' _ _ _ _) -> n') . thrd)
-- | Checks for the existence of a node in a pi
hasNodeP :: N -> Pi -> Bool
hasNodeP n = isJust . findNodeP n
-- | Find a node in a delta list
findNodeD :: N -> Delta -> Maybe D
findNodeD n = find (isN n) . toList
  where isN n' (PlusND (n'', _, _, _, _)) | n' == n'' = True
        isN _ _ = False
-- | Checks for the existence of a node in a delta
hasNodeD :: N -> Delta -> Bool
hasNodeD n = isJust . findNodeD n
-- | Find a edge in a delta list
findEdgeD :: E -> Delta -> Maybe D
findEdgeD e = find (isE e) . toList
  where isE e' (PlusED (e'', _, _, _)) | e' == e'' = True
        isE _ _ = False
-- | Checks for the existence of a node in a delta
hasEdgeD :: E -> Delta -> Bool
hasEdgeD e = isJust . findEdgeD e
-- | Does a delta exist in a delta list
hasDelta :: Delta -> D -> Bool
hasDelta d a = a `elem` toList d
-- | Find edge value at the current level
ev :: E -> Pi -> Upsilon -> Delta -> Maybe V
ev e pi upsilon delta | hasEdge e pi && not (hasDelta delta (MinusE e)) = snd' <$> findEdgePU e pi -- TODO simplify this
                      | hasEdge e upsilon && not (hasDelta delta (MinusE e)) = snd' <$> findEdgePU e upsilon -- TODO simplify this
                      | hasEdgeD e delta = getV <$> findEdgeD e delta
                      | hasNodeD e delta = getV <$> findNodeD e delta
                      | otherwise = Nothing
  where getV (PlusED (_, v, _, _)) = v
getV _ = error "Cannot happen"
-- | Find node value at the current level
nv :: N -> N -> V -> Delta -> Maybe V
nv n n' v delta = if n == n' && not (hasDelta delta (MinusN n))
                  then Just v
                  else getV <$> findNodeD n delta
  where getV (PlusND (_, v', _, _, _)) = v'
        getV _ = error "Cannot happen"
-- | L function, previously called topbacks
lFunction :: Gamma -> N -> (Pi, Delta)
lFunction g n = (iFunction g n, dFunction g n)
-- | I function, edit a gamma for the L function
iFunction :: Gamma -> N -> Pi
iFunction = ihelp Nothing
  where ihelp ed (Gamma n v pi upsilon delta) n' = case findNodeU n' upsilon of
          Just (e, v', _) -> [(e, v', Gamma n v pi (applyRemoveEdgeU e upsilon') delta)]
          Nothing -> piApplyDeltasStarT (concatMap (\(e'', v'', g'') -> flip (ihelp (Just (e'', v'', n))) n' g'') pi) delta
          where upsilon' = case ed of
                  Just (e'', v'', n'') -> applyAddEdgeU (e'', v'', undefined, n'') upsilon
                  Nothing -> upsilon
-- | D function, gather deltas for the L function
dFunction :: Gamma -> N -> Delta
dFunction (Gamma n v pi upsilon delta) n' = if hasNodeU n' upsilon
                                            then Seq.empty
                                            else singleton (UpGamma g')
  where g' = Gamma n v pi' upsilon' delta
        pi' = [(e'', v'', g'') | (e'', v'', g''') <- pi, d' <- toList (dFunction g''' n'), (PlusGamma g'') <- exGamma d']
        upsilon' = upsilon ++ [(e'', v'', n'') | (e'', _, g''') <- pi, null (toList (dFunction g''' n')), (_, v'', n'') <- maybeToList (findEdgePU e'' upsilon)]
exGamma (PlusGamma g) = [PlusGamma g]
exGamma _ = []
-- | fst, snd, and thrd for 3-tuples
fst' :: (a, b, c) -> a
fst' (a, _, _) = a
snd' :: (a, b, c) -> b
snd' (_, b, _) = b
thrd :: (a, b, c) -> c
thrd (_, _, c) = c
-- | Takes a delta off the front of the delta list and applies it
apply :: Gamma -> Gamma
apply g@(Gamma n v pi upsilon delta) | Seq.null delta = g
                                     | otherwise = handleDelta (Gamma n v pi upsilon ds) d
  where d :< ds = viewl delta
-- | Apply all deltas on the top level
applyTop :: Gamma -> Gamma
applyTop g@(Gamma n v pi upsilon delta) | Seq.null delta = g
                                        | otherwise = gammaApplyDeltasH (applyTop g') ups
  where g' = apply (Gamma n v pi upsilon delta')
        (ups, delta') = partition isUpGamma delta
isUpGamma (UpGamma _) = True
isUpGamma _ = False
-- | Apply all deltas on all levels
applyAll :: Gamma -> Gamma
applyAll g@(Gamma _ _ _ _ delta) | Seq.null delta = g
                                 | otherwise = Gamma n' v' (map (\(a, b, c) -> (a, b, applyAll c)) pi') upsilon' delta'
  where Gamma n' v' pi' upsilon' delta' = applyTop g
-- | Helper to handle deltas
-- | Prop cases
handleDelta :: Gamma -> D -> Gamma
handleDelta g@(Gamma n v pi upsilon delta) d@(MinusE e) | hasEdge e pi = Gamma n v pi' upsilon delta'
                                                        | hasEdge e upsilon = Gamma n v pi upsilon' delta
                                                        | otherwise = propD g d
  where pi' = applyRemoveEdgeP e pi
        upsilon' = applyRemoveEdgeU e upsilon
        delta' = case findEdgePU e pi of
          Just (_, _, g') -> delta |> PlusGamma g'
          Nothing -> delta
handleDelta g@(Gamma n v pi upsilon delta) d@(PlusED ed@(_, _, n', _)) | n == n' = Gamma n v pi upsilon' delta
                                                                       | otherwise = propD g d
  where upsilon' = applyAddEdgeU ed upsilon
handleDelta (Gamma n v pi upsilon delta) (MinusN n') | hasNodeP n' pi = Gamma n v pi' upsilon delta
  where Just (e, _, _) = findNodeP n' pi
        pi' = applyRemoveEdgeP e pi
handleDelta (Gamma n v pi upsilon delta) d@(MinusN n') | hasNodeU n' upsilon = Gamma n v pi' upsilon' delta
  where pi' = piApplyDeltaStarT pi d
        upsilon' = applyRemoveNodeU n' upsilon
-- delta' = d <| delta
handleDelta g (MinusN _) = g
handleDelta g@(Gamma n v pi upsilon delta) d@(PlusND (n', v', n'', e, v'')) | n == n'' = Gamma n v pi' upsilon delta
                                                                            | otherwise = propD g d
  where pi' = applyAddEdgeP (e, v'', undefined, undefined) (Gamma n' v' [] [] Seq.empty) pi
handleDelta (Gamma n v pi upsilon delta) (PlusGamma gamma) = Gamma n v (pi ++ pi') upsilon (delta >< delta')
  where (pi', delta') = lFunction gamma n
-- | Add an edge to an upsilon
applyAddEdgeU :: ED -> Upsilon -> Upsilon
applyAddEdgeU (e, v, _, n') = (:) (e, v, n')
-- | Add an edge to an pi
applyAddEdgeP :: ED -> Gamma -> Pi -> Pi
applyAddEdgeP (e, v, _, _) g = (:) (e, v, g)
-- | Remove an edge from an upsilon
applyRemoveEdgeU :: E -> Upsilon -> Upsilon
applyRemoveEdgeU e = filter ((/= e) . fst')
-- | Remove an edge from a pi
applyRemoveEdgeP :: E -> Pi -> Pi
applyRemoveEdgeP e = filter ((/= e) . fst')
-- | Remove a node from an upsilon
applyRemoveNodeU :: N -> Upsilon -> Upsilon
applyRemoveNodeU n = filter ((/= n) . thrd)
--------------------
-- Delta list normalization
--------------------
-- | Normalize a delta list
normalize :: Delta -> Delta
normalize = norm
norm :: Delta -> Delta
norm delta | Seq.null delta || Seq.length delta == 1 = delta
           | otherwise = normHandle d' d ds'
  where ds :> d = viewr delta
        ds' :> d' = viewr ds
-- N-NCancel
        normHandle d'' (MinusN n) delta' | n `elem` addn d'' = delta'
-- N-NUsed
                                         | n `elem` usen d'' = norm (delta' |> MinusN n) >< fromList (MinusN <$> addn d'') >< fromList (MinusE <$> adde d'')
-- N-ECancel1
        normHandle (PlusED (e', _, _, _)) (MinusE e) delta' | e' == e = delta'
-- N-ECancel2
        normHandle (PlusND (n, v, _, e', _)) (MinusE e) delta' | e' == e = delta' |> PlusGamma (Gamma n v [] [] Seq.empty)
-- N-Sub
        normHandle (PlusGamma g) d''@(MinusE e) delta' = norm (delta' |> MinusE e) |> PlusGamma (gammaApplyDeltaT g d'')
-- N-Swap
        normHandle d''' d'' delta' | isPlus d' && isMinus d = norm (delta' |> d'') |> d'''
          where isPlus (PlusND _) = True
                isPlus (PlusED _) = True
                isPlus (PlusGamma _) = True
                isPlus _ = False
                isMinus (MinusN _) = True
                isMinus (MinusE _) = True
                isMinus _ = False
-- Already normalized
        normHandle d''' d'' delta' = delta' |> d''' |> d''
--------------------
-- Exposed commands
--------------------
-- | Delete a node
deleteNode :: Graph -> N -> Graph
deleteNode gr n = gammaApplyDeltaT gr $ MinusN n
-- | Delete an edge
deleteEdge :: Graph -> E -> Graph
deleteEdge gr e = gammaApplyDeltaT gr $ MinusE e
-- | Insert a node
insertNode :: Graph -> ND -> Graph
insertNode gr nd = gammaApplyDeltaT gr $ PlusND nd
-- | Insert a edge
insertEdge :: Graph -> ED -> Graph
insertEdge gr ed = gammaApplyDeltaT gr $ PlusED ed
-- | Find a node
findNode :: Graph -> N -> Maybe V
findNode (Gamma n v pi _ delta) n' = case nv n' n v delta of
  Just v' -> Just v'
  Nothing -> foldl (<|>) Nothing (map (flip findNode n' . thrd) pi)
-- | Find a node in parallel
findNodePar :: Int -> Graph -> N -> Maybe V
findNodePar i (Gamma n v pi _ delta) n' | i <= 0 = case nv n' n v delta of
  Just v' -> Just v'
  Nothing -> foldl' (<|>) Nothing (map (flip findNode n' . thrd) pi)
                                        | otherwise = case nv n' n v delta of
  Just v' -> Just v'
  Nothing -> foldl' (<|>) Nothing (parMap rdeepseq (flip (findNodePar (i-1)) n' . thrd) pi)
-- | Find an edge
findEdge :: Graph -> E -> Maybe V
findEdge (Gamma _ _ pi upsilon delta) e = case ev e pi upsilon delta of
  Just v -> Just v
  Nothing -> foldl (<|>) Nothing (map (flip findEdge e . thrd) pi)
-- | Find an edge in parallel
-- | Find an edge
findEdgePar :: Int -> Graph -> E -> Maybe V
findEdgePar i (Gamma _ _ pi upsilon delta) e | i < 0 = case ev e pi upsilon delta of
  Just v -> Just v
  Nothing -> foldl (<|>) Nothing (map (flip findEdge e . thrd) pi)
                                             | otherwise = case ev e pi upsilon delta of
  Just v -> Just v
  Nothing -> foldl' (<|>) Nothing (parMap rdeepseq (flip (findEdgePar (i-1)) e . thrd) pi)
