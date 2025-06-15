{- Assignment 4
 - Name: Amanda Dam
 - Date: June 13 2021
 -}
module Assign_4 where

macid :: String
macid = "dama5"

{- --------------------------------------------------------------------
 - Datatype: MathExpr
 - --------------------------------------------------------------------
 - Description: An Abstract Syntax Tree (AST) for encoding mathematical
 -              expressions
 - Example: The expression
 -                abs (2*X + 3 + 4)  * X
 -          can be encoded as
 -                Prod [ Abs (Sum [Prod [Coef 2.0,X],Coef 3,Coef 4]),X]
 - --------------------------------------------------------------------
 -}
data MathExpr a =
    X
  | Coef a
  | Sum (MathExpr a) (MathExpr a)
  | Prod (MathExpr a) (MathExpr a)
  | Power (MathExpr a) Int
  | Cos (MathExpr a)
  | Sin (MathExpr a)
  | Abs (MathExpr a)
  deriving (Eq,Show,Read)

{- --------------------------------------------------------------------
 - Function: eval
 - --------------------------------------------------------------------
 - Description: eval takes an expression, e, of type (MathExpr) and a floating point number, v.
 The function returns the value of e at v by subbing in v for X and evaluating. 
 -}
eval ::(Floating a, Eq a) => MathExpr a -> a -> a
eval X v = v
eval (Coef a) v = a
eval (Sum a b) v = eval a v + eval b v
eval (Prod a b) v = eval a v * eval b v
eval (Power a b) v = eval a v ^^ b
eval (Cos a) v = cos (eval a v)
eval (Sin a) v = sin (eval a v)
eval (Abs a) v = abs(eval a v)

{- --------------------------------------------------------------------
 - instance Num a => Num (MathExpr a)
 - --------------------------------------------------------------------
 - Description: Implementatioin of methods for + , * , negate, abs, and fromInteger
 -}
instance Num a => Num (MathExpr a) where
  x + y         = Sum x y
  x * y         = Prod x y
  negate x      = Prod (Coef (-1)) x
  abs x         = Abs x
  fromInteger i = Coef (fromInteger i)
  signum _      = error "signum is left un-implemented"

{- --------------------------------------------------------------------
 - instance Fractional a => Fractional (MathExpr a)
 - --------------------------------------------------------------------
 - Description: Implementation of methods for recip and fromRational
 -}
instance Fractional a => Fractional (MathExpr a) where
  recip e        = Power e (-1)
  fromRational e = Coef (fromRational e)

{- --------------------------------------------------------------------
 - instance Floating a => Floating (MathExpr a)
 - --------------------------------------------------------------------
 - Description: Implementations of methods for pi, sin and cos
 -}
instance Floating a => Floating (MathExpr a) where
  pi      = pi
  sin     = Sin 
  cos     = Cos 
  log     = error "log is left un-implemented"
  asin _  = error "asin is left un-implemented"
  acos _  = error "acos is left un-implemented"
  atan _  = error "atan is left un-implemented"
  sinh _  = error "sinh is left un-implemented"
  cosh _  = error "cosh is left un-implemented"
  tanh _  = error "tanh is left un-implemented"
  asinh _ = error "asinh is left un-implemented"
  acosh _ = error "acosh is left un-implemented"
  atanh _ = error "atanh is left un-implemented"
  exp _   = error "exp is left un-implemented"
  sqrt _  = error "sqrt is left un-implemented"
{- ------------------------------------------------------------------
 - diff
 - ------------------------------------------------------------------
 - Description: Symbolically differentiates an expression of type (MathExpr a)
 -}


diff :: (Floating a, Eq a) => MathExpr a -> MathExpr a
diff X = Coef 1
diff (Coef a) = Coef 0
diff (Sum a b) = diff a + diff b
diff (Prod X (Coef a)) = Coef a
diff (Prod (Coef a) X) = Coef a
diff (Prod a b) = (diff a * b) + (a * diff b)
diff (Power a b) = fromIntegral b * a^^(b-1) * diff a 
diff (Cos a) = negate(sin a) * diff a
diff (Sin a) = cos a * diff a
diff (Abs a)
  | a /= 0 = recip (abs a) * a * diff a
  | otherwise = error "Undefined"

{- -----------------------------------------------------------------
 - pretty
 - -----------------------------------------------------------------
 - Description: Creates a string representation of an expression of type (MathExpr a)
 -}

pretty :: (Show a) => MathExpr a -> String
pretty X = "X"
pretty (Coef c) = show c
pretty (Sum u0 u1) = "(" ++ pretty u0 ++ " + " ++ pretty u1 ++ ")"
pretty (Prod u0 u1) = "(" ++ pretty u0 ++ " * " ++ pretty u1 ++ ")"
pretty (Power u0 d) =  "(" ++ pretty u0 ++ " ^ " ++ show d ++ ")"
pretty (Cos u0) = "cos(" ++ pretty u0 ++ ")"
pretty (Sin u0) = "sin(" ++ pretty u0 ++ ")"
pretty (Abs u0) = "abs(" ++ pretty u0 ++ ")"

{- -----------------------------------------------------------------
 - Test Cases
 - -----------------------------------------------------------------
 -}

-- TODO Add Test Cases for each of your functions below here

test :: Floating a => MathExpr a
test = X*4 + (X+2)^2