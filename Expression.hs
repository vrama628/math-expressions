module Expression where

infixl 6 :+:, :-:
infixl 7 :*:, :/:
data Expression a = Val a
                  | Var String
                  | (Expression a) :+: (Expression a)
                  | (Expression a) :-: (Expression a)
                  | (Expression a) :*: (Expression a)
                  | (Expression a) :/: (Expression a)

--  show outputs the expression written in mathematical notation
instance Show a => Show (Expression a) where
  showsPrec _ (Val x) = shows x
  showsPrec _ (Var n) = shows n
  showsPrec p (e :+: f) = showParen (p > 6) $
    showsPrec 6 e . ('+':) . showsPrec 6 f
  showsPrec p (e :-: f) = showParen (p > 6) $
    showsPrec 6 e . ('-':) . showsPrec 6 f
  showsPrec p (e :*: f) = showParen (p > 7) $
    showsPrec 7 e . ('*':) . showsPrec 7 f
  showsPrec p (e :/: f) = showParen (p > 7) $
    showsPrec 7 e . ('*':) . showsPrec 7 f

--  Equality is defined recursively in the natural way
instance Eq a => Eq (Expression a) where
  Val x == Val y = x == y
  Var m == Var n = m == n
  e :+: f == g :+: h = (e == g) && (f == h)
  e :-: f == g :-: h = (e == g) && (f == h)
  e :*: f == g :*: h = (e == g) && (f == h)
  e :/: f == g :/: h = (e == g) && (f == h)
  _ == _ = False

--  The Simplifiable typeclass allows for defining different simplification
--  behaviors for Expressions of different numerical types. For example,
--  floating-point division is used to simplify divison of Fractional numbers,
--  while fraction reduction is used to simplify division of Integral numbers.
class Num a => Simplifiable a where
  over :: a -> a -> Expression a

overFractional :: Fractional a => a -> a -> Expression a
x `overFractional` y = Val $ x / y

overIntegral :: Integral a => a -> a -> Expression a
x `overIntegral` y =  let g = gcd x y
                      in  if y == g
                            then Val $ x `div` g
                            else (Val $ x `div` g) :/: (Val $ y `div` g)

instance Simplifiable Float   where over = overFractional
instance Simplifiable Double  where over = overFractional
instance Simplifiable Int     where over = overIntegral
instance Simplifiable Integer where over = overIntegral

-- the at function allows you to assign a value to a variable.
type Assignment a = (String, a)
infixl 5 `at`
at :: Expression a -> Assignment a -> Expression a
Var m `at` (n, k) = if m == n then Val k else Var m
Val k `at` _ = Val k
e :+: f `at` (n, k) = (e `at` (n, k)) :+: (f `at` (n, k))
e :-: f `at` (n, k) = (e `at` (n, k)) :-: (f `at` (n, k))
e :*: f `at` (n, k) = (e `at` (n, k)) :*: (f `at` (n, k))
e :/: f `at` (n, k) = (e `at` (n, k)) :/: (f `at` (n, k))

atAll :: Expression a -> [Assignment a] -> Expression a
atAll = foldl at