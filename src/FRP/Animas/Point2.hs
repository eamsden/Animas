{-# LANGUAGE MultiParamTypeClasses, FlexibleInstances #-}
-- |
-- Module      :  FRP.Animas.Point2
-- Copyright   :  (c) Antony Courtney and Henrik Nilsson, Yale University, 2003
-- License     :  BSD-style (see the LICENSE file in the distribution)
--
-- Maintainer  :  nilsson@cs.yale.edu
-- Stability   :  provisional
-- Portability :  non-portable (GHC extensions)
--
-- 2D point abstraction (R^2).
--
-- ToDo: Deriving Show, or provide dedicated show instance?
--

module FRP.Animas.Point2 (
    Point2(..),
    point2X,
    point2Y
) where

import FRP.Animas.VectorSpace ()
import FRP.Animas.AffineSpace
import FRP.Animas.Vector2
import FRP.Animas.Forceable

-- | Two-dimensional real-valued point
data RealFloat a => Point2 a = Point2 !a !a deriving (Eq, Show)

-- | X coordinate
point2X :: RealFloat a => Point2 a -> a
point2X (Point2 x _) = x

-- | Y coordinate
point2Y :: RealFloat a => Point2 a -> a
point2Y (Point2 _ y) = y


instance RealFloat a => AffineSpace (Point2 a) (Vector2 a) a where
    origin = Point2 0 0

    (Point2 x y) .+^ v = Point2 (x + vector2X v) (y + vector2Y v)

    (Point2 x y) .-^ v = Point2 (x - vector2X v) (y - vector2Y v)

    (Point2 x1 y1) .-. (Point2 x2 y2) = vector2 (x1 - x2) (y1 - y2)

instance RealFloat a => Forceable (Point2 a) where
     force = id
