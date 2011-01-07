{-# LANGUAGE MultiParamTypeClasses, FlexibleInstances #-}
-- |
-- Module      :  FRP.Yampa.Point3
-- Copyright   :  (c) Antony Courtney and Henrik Nilsson, Yale University, 2003
-- License     :  BSD-style (see the LICENSE file in the distribution)
--
-- Maintainer  :  nilsson@cs.yale.edu
-- Stability   :  provisional
-- Portability :  non-portable (GHC extensions)
--
-- 3D point abstraction (R^3).
--

module FRP.Yampa.Point3 (
    Point3(..),
    point3X,
    point3Y,
    point3Z
) where

import FRP.Yampa.VectorSpace ()
import FRP.Yampa.AffineSpace
import FRP.Yampa.Vector3
import FRP.Yampa.Forceable

-- | 3-dimensional, real-valued point
data RealFloat a => Point3 a = Point3 !a !a !a deriving Eq

-- | X coordinate
point3X :: RealFloat a => Point3 a -> a
point3X (Point3 x _ _) = x

-- | Y coordinate
point3Y :: RealFloat a => Point3 a -> a
point3Y (Point3 _ y _) = y

-- | Z coordinate
point3Z :: RealFloat a => Point3 a -> a
point3Z (Point3 _ _ z) = z

instance RealFloat a => AffineSpace (Point3 a) (Vector3 a) a where
    origin = Point3 0 0 0

    (Point3 x y z) .+^ v =
	Point3 (x + vector3X v) (y + vector3Y v) (z + vector3Z v)

    (Point3 x y z) .-^ v =
	Point3 (x - vector3X v) (y - vector3Y v) (z - vector3Z v)

    (Point3 x1 y1 z1) .-. (Point3 x2 y2 z2) =
	vector3 (x1 - x2) (y1 - y2) (z1 - z2)

instance RealFloat a => Forceable (Point3 a) where
     force = id
