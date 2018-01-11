module Main

import Data.Vect

import Control.Algebra
import Control.Algebra.NumericImplementations

%default total
%access public export
{-
Semigroup () where
  (<+>) () () = ()

Monoid () where
  neutral = ()

Group () where
  inverse = id
-}

interface NRoot a where
  nroot : a -> Int -> a
  fpow : a -> a -> a

sqrt : NRoot a => a -> a
sqrt a = nroot a 2

NRoot Double where
  nroot d i = pow d (1.0/cast i)
  fpow = pow

Semigroup a => Semigroup (Vect n a) where
  (<+>) = zipWith (<+>) 
  
Monoid a => Monoid (Vect n a) where
  neutral {n} = replicate n neutral
  
Group a => Group (Vect n a) where
  inverse = map inverse   

infixr 7 *^

interface Group v => VecSpace v where
  Scalar : Type
  (*^) : Scalar -> v -> v

VecSpace Int where
  Scalar = Int
  (*^) = (*)

VecSpace Double where
  Scalar = Double
  (*^) = (*)

VecSpace v => VecSpace (Vect n v) where
  Scalar {v} = Scalar {v}
  (*^) {v} s = map ((*^) {v} s)

infixr 7 <&>
interface (VecSpace v, Group (Scalar {v})) => InnerSpace v where 
    -- Inner/dot product
  (<&>) : v -> v -> Scalar {v}

InnerSpace Double where
  (<&>) = (*)

-- how to pass the group reqmnt automatically?  
(InnerSpace v, Group (Scalar {v})) => InnerSpace (Vect n v) where
  (<&>) xs ys = concat $ zipWith (<&>) xs ys
    

infixr 7 ^/
-- Vector divided by scalar
-- TODO Field instead of Fractional?
(^/) : (VecSpace v, Fractional (Scalar {v})) => v -> Scalar {v} -> v
(^/) v s = (1/s) *^ v

infixl 7 ^*
-- Vector multiplied by scalar
(^*) : VecSpace v => v -> Scalar {v} -> v
(^*) = flip (*^)

-- Linear interpolation 
lerp : VecSpace v => v -> v -> Scalar {v} -> v
lerp a b t = a <+> t *^ (b <-> a)

-- Linear combination of vectors
lincomb : VecSpace v => List (v, Scalar {v}) -> v
lincomb ps = concat [v ^* s | (v,s) <- ps]

-- Square of the length of a vector.  Sometimes useful for efficiency.
magnitudeSq : InnerSpace v => v -> Scalar {v}
magnitudeSq v = v <&> v

-- Length of a vector.   See also 'magnitudeSq'.
magnitude : (InnerSpace v, NRoot (Scalar {v})) => v -> Scalar {v}
magnitude = sqrt . magnitudeSq

-- Vector in same direction as given one but with length of one. 
-- TODO If given the zero vector, then return it.
normalized : (InnerSpace v, Fractional (Scalar {v}), NRoot (Scalar {v})) => v -> v
normalized v = v ^/ magnitude v

-- The projection of v onto u.
project : (InnerSpace v, Fractional (Scalar {v})) => v -> v -> v
project u v = ((v <&> u) / magnitudeSq u) *^ u

interface VecSpace v => HasBasis v where
  Basis : Type
  basisValue : Basis -> v
  decompose : v -> List (Basis, Scalar {v})
  decompose' : v -> Basis -> Scalar {v}

-- TODO profunctors? doesn't seem to work with Control.Arrow  
first : (a -> c) -> (a, b) -> (c, b)
first f = \(a, b) => (f a, b)
   
recompose : HasBasis v => List (Basis {v}, Scalar {v}) -> v 
recompose = lincomb . map (first basisValue)
  

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

--type LMap' u v = MSum (Basis u :->: v)
  

