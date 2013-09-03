module Proxy.Paul.BSTree where

{- Implementation of BST (binary search tree)
Script is absolutly free/libre, but with no guarantee.
Author: Ondrej Profant -}
 
import qualified Data.List
 
{- DEF data structure -}
data Tree a = Nil | Node (Tree a) a (Tree a) 
	deriving Show
                 
instance Functor Tree where
  fmap f Nil = Nil
  fmap f (Node xt x yt) = Node (fmap f xt) (f x) (fmap f yt)
 
{- BASIC Information -}
empty :: (Ord a) => Tree a -> Bool
empty Nil = True
empty  _  = False
 
contains :: (Ord a) => (Tree a) -> a -> Bool
contains Nil _ = False
contains (Node t1 v t2) x 
	| x == v = True
	| x  < v = contains t1 x 
	| x  > v = contains t2 x
 
{- BASIC Manipulation -}
insert :: (Ord a) => Tree a -> a -> Tree a
insert Nil x = Node Nil x Nil
insert (Node t1 v t2) x 
	| v == x = Node t1 v t2
	| v  < x = Node t1 v (insert t2 x)
	| v  > x = Node (insert t1 x) v t2
 
delete :: (Ord a) => Tree a -> a -> Tree a
delete Nil _ = Nil
delete (Node t1 v t2) x  
	| x == v = deleteX (Node t1 v t2)
	| x  < v = Node (delete t2 x) v t2
	| x  > v = Node t1 v (delete t2 x)
 
-- Delete root (is used on subtree)
deleteX :: (Ord a) => Tree a -> Tree a 
deleteX (Node Nil v t2) = t2
deleteX (Node t1 v Nil) = t1
deleteX (Node t1 v t2) = (Node t1 v2 t2) --(delete t2 v2))
	where 
		v2 = leftistElement t2
 
-- Return leftist element of tree (is used on subtree)
leftistElement :: (Ord a) => Tree a -> a
leftistElement (Node Nil v _) = v
leftistElement (Node t1 _ _) = leftistElement t1
 
-- Create tree from list of elemtents
ctree :: (Ord a) => [a] -> Tree a
ctree [] = Nil
ctree (h:t) = ctree2 (Node Nil h Nil) t
	where
		ctree2 tr [] = tr
		ctree2 tr (h:t) = ctree2 (insert tr h) t
 
-- Create perfect balance BST
ctreePB :: (Ord a) => [a] -> Tree a
ctreePB [] = Nil
ctreePB s = cpb Nil (qsort s) 
 
cpb :: (Ord a) => Tree a -> [a] -> Tree a
cpb tr [] = tr
cpb tr t = cpb (insert tr e) t2
	where	
		e = middleEl t
		t2 = Data.List.delete e t
 
-- Element in middle
middleEl :: (Ord a) => [a] -> a
middleEl s = mEl s s 
 
mEl :: (Ord a) => [a] ->  [a] -> a
mEl    []    (h:s2) = h
mEl (_:[])   (h:s2) = h
mEl (_:_:s1) (_:s2) = mEl s1 s2
 
{- PRINT -}
inorder :: (Ord a) => Tree a -> [a]
inorder Nil = []
inorder (Node t1 v t2) = inorder t1 ++ [v] ++ inorder t2
 
preorder :: (Ord a) => Tree a -> [a]
preorder Nil = []
preorder (Node t1 v t2) = [v] ++ preorder t1 ++ preorder t2
 
postorder :: (Ord a) => Tree a -> [a]
postorder Nil = []
postorder (Node t1 v t2) = postorder t1 ++ postorder t2 ++ [v]
 
-- from wiki
levelorder :: (Ord a) => Tree a -> [a]
levelorder t = step [t]
	where
		step [] = []
		step ts = concatMap elements ts ++ step (concatMap subtrees ts)
		elements Nil = []
		elements (Node left x right) = [x]
		subtrees Nil = []
		subtrees (Node left x right) = [left,right]
 
qsort :: (Ord a) => [a] -> [a] 
qsort [] = []
qsort (h:t) = (qsort [x| x<-t, x < h]) ++ [h] ++ (qsort [x| x<-t, x>=h ])