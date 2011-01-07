{-# LANGUAGE MultiParamTypeClasses, FlexibleInstances #-}
-- |
-- Module      :  FRP.Yampa.Vector2
-- Copyright   :  (c) Antony Courtney and Henrik Nilsson, Yale University, 2003
-- License     :  BSD-style (see the LICENSE file in the distribution)
--
-- Maintainer  :  nilsson@cs.yale.edu
-- Stability   :  provisional
-- Portability :  non-portable (GHC extensions)
--
-- 2D vector abstraction (R^2).
--
-- ToDo: Deriving Show, or provide dedicated show instance?

module FRP.Yampa.Vector2 (
    Vector2,
    vector2,
    vector2X,
    vector2Y,
    vector2XY,
    vector2Polar,
    vector2Rho,
    vector2Theta,
    vector2RhoTheta,
    vector2Rotate
) where

import FRP.Yampa.VectorSpace
import FRP.Yampa.Forceable

-- | 2-dimensional vector type
data RealFloat a => Vector2 a = Vector2 !a !a deriving (Eq,Show)

-- | Create a 2D vector
vector2 :: RealFloat a 
           => a -- ^ X magnitude
           -> a -- ^ Y magnitude
           -> Vector2 a -- ^ Vector
vector2 x y = Vector2 x y

-- ^ Obtain the X-magnitude of a vector
vector2X :: RealFloat a => Vector2 a -> a
vector2X (Vector2 x _) = x

-- ^ Obtain the Y-magnitude of a vector
vector2Y :: RealFloat a => Vector2 a -> a
vector2Y (Vector2 _ y) = y

-- ^ Obtain the X and Y magnitudes of a vector as an ordered pair
vector2XY :: RealFloat a => 
             Vector2 a 
             -> (a, a) -- ^ (X, Y)
vector2XY (Vector2 x y) = (x, y)

-- ^ Create a vector from polar coordinates (magnitude/rho, direction/theta (radians))
vector2Polar :: RealFloat a => 
                a -- ^ Rho
                -> a -- ^ Theta
                -> Vector2 a -- ^ Vector
vector2Polar rho theta = Vector2 (rho * cos theta) (rho * sin theta) 

-- ^ Obtain the magnitude of a vector
vector2Rho :: RealFloat a => Vector2 a -> a
vector2Rho (Vector2 x y) = sqrt (x * x + y * y)

-- ^ Obtain the direction of a vector
vector2Theta :: RealFloat a => Vector2 a -> a
vector2Theta (Vector2 x y) = atan2 y x

-- ^ Obtain the magnitude and direction of a vector as an ordered pair
vector2RhoTheta :: RealFloat a => 
                   Vector2 a 
                   -> (a, a) -- ^ (Rho, Theta)
vector2RhoTheta v = (vector2Rho v, vector2Theta v)

instance RealFloat a => VectorSpace (Vector2 a) a where
    zeroVector = Vector2 0 0

    a *^ (Vector2 x y) = Vector2 (a * x) (a * y)

    (Vector2 x y) ^/ a = Vector2 (x / a) (y / a)

    negateVector (Vector2 x y) = (Vector2 (-x) (-y))

    (Vector2 x1 y1) ^+^ (Vector2 x2 y2) = Vector2 (x1 + x2) (y1 + y2)

    (Vector2 x1 y1) ^-^ (Vector2 x2 y2) = Vector2 (x1 - x2) (y1 - y2)

    (Vector2 x1 y1) `dot` (Vector2 x2 y2) = x1 * x2 + y1 * y2


-- ^ Rotate a vector by some angle theta
vector2Rotate :: RealFloat a => a -- ^ Theta (radians)
                 -> Vector2 a -- ^ Initial vector
                 -> Vector2 a -- ^ Rotated vector
vector2Rotate theta' v = vector2Polar (vector2Rho v) (vector2Theta v + theta')

instance RealFloat a => Forceable (Vector2 a) where
     force = id
