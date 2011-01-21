{-# LANGUAGE MultiParamTypeClasses, FunctionalDependencies, FlexibleInstances #-}
-- |
-- Module      :  FRP.Animas.AffineSpace
-- Copyright   :  (c) Antony Courtney and Henrik Nilsson, Yale University, 2003
-- License     :  BSD-style (see the LICENSE file in the distribution)
--
-- Maintainer  :  nilsson@cs.yale.edu
-- Stability   :  provisional
-- Portability :  non-portable (GHC extensions)
--
-- Affine space type relation.

module FRP.Animas.AffineSpace where

import FRP.Animas.VectorSpace

infix 6 .+^, .-^, .-.

-- | Typeclass for an Affine space.
-- Minimal complete definition: 'origin', '(.+^)', '(.-.)' 
class (Floating a, VectorSpace v a) => AffineSpace p v a | p -> v, v -> a where
    -- | The origin value of an affine space
    origin   :: p
    -- | Add a vector to a point, obtaining a new point.
    (.+^)    :: p -> v -> p
    -- | Subtract a vector from a point, obtaining a new point.
    (.-^)    :: p -> v -> p
    -- | Take the difference of two points, returning a vector
    (.-.)    :: p -> p -> v
    -- | The scalar distance between two points.
    distance :: p -> p -> a

    p .-^ v = p .+^ (negateVector v)

    distance p1 p2 = norm (p1 .-. p2)
