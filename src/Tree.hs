{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE PolyKinds#-}
-- {-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE UndecidableInstances #-}
-- {-# LANGUAGE RankNTypes #-}
{-# LANGUAGE Rank2Types #-}
{-# LANGUAGE TemplateHaskell #-}
-- {-# LANGUAGE PolyKinds #-}
{-# LANGUAGE DeriveFoldable #-}
{-# LANGUAGE DeriveTraversable #-}
{-# LANGUAGE DeriveFunctor #-}
-- {-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
-- {-# LANGUAGE DeriveApplicative #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE RecordWildCards#-}
module Tree where

data Tree a = Leaf
            | Branch
            { leftTree  :: Tree a
            , value     :: a
            , rightTree :: Tree a
            } deriving (Functor, Foldable, Traversable)

instance (Show a) => Show (Tree a) where
    show t = prettyPrint t "" "< " where
        leftTag = " /"
        rightTag = " \\"
        prettyPrint Leaf _ _ = ""
        prettyPrint (Branch lt x rt) linePrefix nodeTag = 
            prettyPrint lt leftPrefix leftTag ++ 
            (linePrefix ++ nodeTag ++ show x ++ "\n") ++ 
            prettyPrint rt rightPrefix rightTag where
                leftPrefix
                  | nodeTag == leftTag = linePrefix ++ "  "
                  | nodeTag == rightTag = linePrefix ++ "| "
                  | otherwise = "  "
                rightPrefix 
                  | nodeTag == leftTag = linePrefix ++ "| "
                  | nodeTag == rightTag = linePrefix ++ "  "
                  | otherwise = "  "

insert :: Ord a => a -> Tree a -> Tree a
insert x Leaf = Branch Leaf x Leaf
insert x (Branch lt x' rt)
  | x < x' = Branch (insert x lt) x' rt
  | x > x' = Branch lt x' (insert x rt)
  | otherwise = Branch lt x' rt


mkTree :: Ord a => [a] -> Tree a
mkTree xs = foldl (flip insert) Leaf xs

rotateRight :: Tree a -> Tree a
rotateRight (Branch (Branch lt' x' rt') x rt) = Branch lt' x' (Branch rt' x rt)
rotateRight t = t

rotateLeft :: Tree a -> Tree a
rotateLeft (Branch lt x (Branch lt' x' rt')) = Branch (Branch lt x lt') x' rt'

treeHeight :: Tree a -> Int
treeHeight Leaf = 0
treeHeight (Branch lt _ rt) = 1 + max (treeHeight lt) (treeHeight rt)

balanceFactor :: Tree a -> Int
balanceFactor Leaf = 0
balanceFactor (Branch lt _ rt) = (treeHeight lt) - (treeHeight rt)

balanceTree :: Tree a -> Tree a
balanceTree Leaf = Leaf
balanceTree Branch{..}
  | balanceFactor st < (-1) = balanceTree (rotateLeft st)
  | balanceFactor st > 1    = balanceTree (rotateRight st)
  | otherwise = st
    where
        st = Branch (balanceTree leftTree) value (balanceTree rightTree)

