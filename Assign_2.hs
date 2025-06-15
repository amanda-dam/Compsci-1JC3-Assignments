{- Assignment 2
 - Name: Amanda Dam
 - Date: May 23 2020
 -}
module Assign_2 where

macid :: String
macid = "dama5"

type Euclid2D = (Double,Double)

{- getX returns the x value of a given Euclid2D, getY returns the y value of a given Euclid2D -}

getX :: Euclid2D -> Double
getX e = fst e

getY :: Euclid2D -> Double
getY e = snd e

{- scalarMult returns the scalar multiplication of a double and Euclid2D -}

scalarMult :: Double -> Euclid2D -> Euclid2D
scalarMult x e = (getX e * x, getY e * x)

{- add computes the 2D vector addition of two Euclid2D -}

add :: Euclid2D -> Euclid2D -> Euclid2D
add e1 e2 = (getX e1 + getX e2, getY e1 + getY e2)

{- innerProduct computes the inner product for 2D Euclidean space (takes two Euclid2D) -}

innerProduct :: Euclid2D -> Euclid2D -> Double
innerProduct e1 e2 = getX e1 * getX e2 + getY e1 * getY e2

{- distance computes the Euclidean distance between two Euclid2D -}

distance :: Euclid2D -> Euclid2D -> Double
distance e1 e2 = let
                    e_2 = scalarMult (-1) e2
                    e_final = add e1 e_2
                    inside = innerProduct e_final e_final
                in sqrt inside

{-  Given a list of elements of type Euclid2D, maxDistance returns the element with the largest distance from (0,0) -}

maxDistance :: [Euclid2D] -> Euclid2D
maxDistance [] = (0,0)
maxDistance (e:es)
  | distance (0,0) e >= distance (0,0) (maxDistance es) = e
  | otherwise = maxDistance es
