module Instances

import Data.Vect
import Control.Algebra
import Control.Algebra.NumericImplementations

import Util
import VecSpace

%default total
%access public export

Semigroup () where
  (<+>) () () = ()

Monoid () where
  neutral = ()

Group () where
  inverse = id

NRoot Double where
  nroot d i = pow d (1.0/cast i)
  fpow = pow

Semigroup a => Semigroup (Vect n a) where
  (<+>) = zipWith (<+>) 
  
Monoid a => Monoid (Vect n a) where
  neutral {n} = replicate n neutral
  
Group a => Group (Vect n a) where
  inverse = map inverse

VecSpace Int where
  Scalar = Int
  (*^) = (*)

VecSpace Double where
  Scalar = Double
  (*^) = (*)

VecSpace v => VecSpace (Vect n v) where
  Scalar {v} = Scalar {v}
  (*^) {v} s = map ((*^) {v} s)

InnerSpace Double where
  (<&>) = (*)

-- how to pass the group reqmnt automatically?  
(InnerSpace v, Group (Scalar {v})) => InnerSpace (Vect n v) where
  (<&>) xs ys = concat $ zipWith (<&>) xs ys    