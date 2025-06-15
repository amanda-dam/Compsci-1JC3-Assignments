{- Assignment 1
 - Name: Amanda Dam
 - Date: May 16, 2021
 -}
module Assign_1 where

macid :: String
macid = "dama5"


factorial :: Integer -> Integer
factorial n = if n > 0
              then n * factorial (n-1)
              else 1

{- cosTaylor computes the 4th Taylor polynomial approximation of cos(x) at a -}

cosTaylor :: Double -> Double -> Double -> Double -> Double
cosTaylor a cos_a sin_a x = (cos_a/ fromIntegral(factorial 0)) + ((-sin_a/ fromIntegral(factorial 1)) * (x-a))+ ((-cos_a / fromIntegral(factorial 2)) * (x-a)^2) + ((sin_a / fromIntegral(factorial 3)) * (x-a)^3) + ((cos_a/ fromIntegral(factorial 4)) * (x-a)^4)

{- fmod computes mod of two real numbers as doubles-}

fmod :: Double -> Double -> Double
fmod x y = x - fromIntegral(floor(x/y)) * y

{- cosApprox computes an approximation of cos(x) using cosTaylor-}
cosApprox :: Double -> Double
cosApprox x
    | y >= 0 && y < pi/4 = cosTaylor 0.0 1.0 0.0 y
    | y >= pi/4 && y < (3*pi/4) = cosTaylor (pi/2) 0.0 1.0 y
    | y >= 3*pi/4 && y < (5*pi/4) = cosTaylor pi (-1.0) 0.0 y
    | y >= 5*pi/4 && y < (7*pi/4) = cosTaylor (3*pi/2) 0.0 (-1.0) y
    | y >= 7*pi/4 && y < (2*pi) = cosTaylor (2*pi) 1.0 0.0 y
    where y = fmod (abs x) (2*pi)
          
{- sinApprox computes an approximation of sin(x) using cosApprox -}
sinApprox :: Double -> Double
sinApprox x = -cosApprox(x + pi/2)
            
{- tanApprox computes an approximation of tan(x) using cosApprox and sinApprox-}
tanApprox :: Double -> Double
tanApprox x
    | cosApprox x == 0 = error "undefined"
    | otherwise = sinApprox x / cosApprox x
