{-# LANGUAGE MultiParamTypeClasses, FunctionalDependencies, FlexibleInstances #-}
-- |
-- Module      :  FRP.Yampa.VectorSpace
-- Copyright   :  (c) Antony Courtney and Henrik Nilsson, Yale University, 2003
-- License     :  BSD-style (see the LICENSE file in the distribution)
--
-- Maintainer  :  nilsson@cs.yale.edu
-- Stability   :  provisional
-- Portability :  non-portable (GHC extensions)
--
-- Vector space type relation and basic instances.

module FRP.Yampa.VectorSpace where

infixr *^
infixl ^/
infix 7 `dot`
infixl 6 ^+^, ^-^

-- | Type class for a vector space
class Floating a => VectorSpace v a | v -> a where
    -- | Vector with no magnitude
    zeroVector   :: v
    -- | Scale the magnitude
    (*^)         :: a -> v -> v
    -- | De-scale the magnitude
    (^/)         :: v -> a -> v
    -- | Negation
    negateVector :: v -> v
    -- | Combine two vectors additively
    (^+^)        :: v -> v -> v
    -- | Subtract a vector from another
    (^-^)        :: v -> v -> v
    -- | Take the dot-product of two vectors
    dot          :: v -> v -> a
    -- | Vector norm
    norm	 :: v -> a
    -- | Produce a unit vector in the direction of a vector
    normalize	 :: v -> v

    v ^/ a = (1/a) *^ v

    negateVector v = (-1) *^ v

    v1 ^-^ v2 = v1 ^+^ negateVector v2

    norm v = sqrt (v `dot` v)

    normalize v = if nv /= 0 then v ^/ nv else error "normalize: zero vector"
        where
	    nv = norm v

instance VectorSpace Float Float where
    zeroVector = 0

    a *^ x = a * x

    x ^/ a = x / a

    negateVector x = (-x)

    x1 ^+^ x2 = x1 + x2

    x1 ^-^ x2 = x1 - x2

    x1 `dot` x2 = x1 * x2


instance VectorSpace Double Double where
    zeroVector = 0

    a *^ x = a * x

    x ^/ a = x / a

    negateVector x = (-x)

    x1 ^+^ x2 = x1 + x2

    x1 ^-^ x2 = x1 - x2

    x1 `dot` x2 = x1 * x2


instance Floating a => VectorSpace (a,a) a where
    zeroVector = (0,0)

    a *^ (x,y) = (a * x, a * y)

    (x,y) ^/ a = (x / a, y / a)

    negateVector (x,y) = (-x, -y)

    (x1,y1) ^+^ (x2,y2) = (x1 + x2, y1 + y2)

    (x1,y1) ^-^ (x2,y2) = (x1 - x2, y1 - y2)

    (x1,y1) `dot` (x2,y2) = x1 * x2 + y1 * y2


instance Floating a => VectorSpace (a,a,a) a where
    zeroVector = (0,0,0)

    a *^ (x,y,z) = (a * x, a * y, a * z)

    (x,y,z) ^/ a = (x / a, y / a, z / a)

    negateVector (x,y,z) = (-x, -y, -z)

    (x1,y1,z1) ^+^ (x2,y2,z2) = (x1+x2, y1+y2, z1+z2)

    (x1,y1,z1) ^-^ (x2,y2,z2) = (x1-x2, y1-y2, z1-z2)

    (x1,y1,z1) `dot` (x2,y2,z2) = x1 * x2 + y1 * y2 + z1 * z2


instance Floating a => VectorSpace (a,a,a,a) a where
    zeroVector = (0,0,0,0)

    a *^ (x,y,z,u) = (a * x, a * y, a * z, a * u)

    (x,y,z,u) ^/ a = (x / a, y / a, z / a, u / a)

    negateVector (x,y,z,u) = (-x, -y, -z, -u)

    (x1,y1,z1,u1) ^+^ (x2,y2,z2,u2) = (x1+x2, y1+y2, z1+z2, u1+u2)

    (x1,y1,z1,u1) ^-^ (x2,y2,z2,u2) = (x1-x2, y1-y2, z1-z2, u1-u2)

    (x1,y1,z1,u1) `dot` (x2,y2,z2,u2) = x1 * x2 + y1 * y2 + z1 * z2 + u1 * u2


instance Floating a => VectorSpace (a,a,a,a,a) a where
    zeroVector = (0,0,0,0,0)

    a *^ (x,y,z,u,v) = (a * x, a * y, a * z, a * u, a * v)

    (x,y,z,u,v) ^/ a = (x / a, y / a, z / a, u / a, v / a)

    negateVector (x,y,z,u,v) = (-x, -y, -z, -u, -v)

    (x1,y1,z1,u1,v1) ^+^ (x2,y2,z2,u2,v2) = (x1+x2, y1+y2, z1+z2, u1+u2, v1+v2)

    (x1,y1,z1,u1,v1) ^-^ (x2,y2,z2,u2,v2) = (x1-x2, y1-y2, z1-z2, u1-u2, v1-v2)

    (x1,y1,z1,u1,v1) `dot` (x2,y2,z2,u2,v2) =
        x1 * x2 + y1 * y2 + z1 * z2 + u1 * u2 + v1 * v2