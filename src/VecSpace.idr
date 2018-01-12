module Instances

import Control.Algebra

import Util

%default total
%access public export

-- Vector space 

infixr 7 *^
interface Group v => VecSpace v where
  Scalar : Type
  (*^) : Scalar -> v -> v

infixr 7 ^/
-- Vector divided by scalar
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

-- Inner product space

infixr 7 <&>
interface (VecSpace v, Group (Scalar {v})) => InnerSpace v where 
    -- Inner/dot product
  (<&>) : v -> v -> Scalar {v}

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

-- Basis of vector space

interface VecSpace v => HasBasis v where
  Basis : Type
  basisValue : Basis -> v
  decompose : v -> List (Basis, Scalar {v})
  decompose' : v -> Basis -> Scalar {v}  

recompose : HasBasis v => List (Basis {v}, Scalar {v}) -> v 
recompose = lincomb . map (first basisValue)

--type LMap' u v = MSum (Basis u :->: v)
