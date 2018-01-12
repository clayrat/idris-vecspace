module Util

import Control.Algebra

%default total
%access public export

interface NRoot a where
  nroot : a -> Int -> a
  fpow : a -> a -> a

sqrt : NRoot a => a -> a
sqrt a = nroot a 2

-- TODO profunctors? doesn't seem to work with Control.Arrow  
first : (a -> c) -> (a, b) -> (c, b)
first f = \(a, b) => (f a, b)

data Sum a = MkSum a 

getSum : Sum a -> a 
getSum (MkSum a) = a

Functor Sum where
  map f (MkSum a) = MkSum (f a)

infixr 7 ~>
  
(~>) : (a' -> a) -> (b -> b') -> ((a -> b) -> (a' -> b'))
(~>) i o f = o . f . i
    
-- | Application a unary function inside a 'Sum'
inSum : (a -> b) -> (Sum a -> Sum b)
inSum = getSum ~> MkSum

-- | Application a binary function inside a 'Sum'
inSum2 : (a -> b -> c) -> (Sum a -> Sum b -> Sum c)
inSum2 = getSum ~> inSum
  
Applicative Sum where
  pure = MkSum
  (<*>) = inSum2 apply

Semigroup a => Semigroup (Sum a) where
  (<+>) = liftA2 (<+>)

Monoid a => Monoid (Sum a) where
  neutral = MkSum neutral

Group a => Group (Sum a) where
  inverse = inSum inverse

MSum : Type -> Type 
MSum a = Maybe (Sum a)

jsum : a -> MSum a
jsum = Just . MkSum