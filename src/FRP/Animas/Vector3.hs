{-# LANGUAGE FlexibleInstances, MultiParamTypeClasses #-}
-- |
-- Module      :  FRP.Animas.Vector3
-- Copyright   :  (c) Antony Courtney and Henrik Nilsson, Yale University, 2003
-- License     :  BSD-style (see the LICENSE file in the distribution)
--
-- Maintainer  :  nilsson@cs.yale.edu
-- Stability   :  provisional
-- Portability :  non-portable (GHC extensions)
--
-- 3D vector abstraction (R^3).
--
-- ToDo: Deriving Show, or provide dedicated show instance?

module FRP.Animas.Vector3 (

    Vector3,
    vector3,
    vector3X,
    vector3Y,
    vector3Z,
    vector3XYZ,
    vector3Spherical,
    vector3Rho,
    vector3Theta,
    vector3Phi,
    vector3RhoThetaPhi,
    vector3Rotate
) where

import FRP.Animas.VectorSpace
import FRP.Animas.Forceable

-- | 3-dimensional vector
data RealFloat a => Vector3 a = Vector3 !a !a !a deriving (Eq, Show)

-- | Construct a 3 dimensional vector
vector3 :: RealFloat a => a -- ^ X magnitude
           -> a -- ^ Y magnitude
           -> a -- ^ Z magnitude
           -> Vector3 a -- ^ Vector
vector3 x y z = Vector3 x y z

-- | X magnitude of the vector
vector3X :: RealFloat a => Vector3 a -> a
vector3X (Vector3 x _ _) = x

-- | Y magnitude of the vector
vector3Y :: RealFloat a => Vector3 a -> a
vector3Y (Vector3 _ y _) = y

-- | Z magnitude of the vector
vector3Z :: RealFloat a => Vector3 a -> a
vector3Z (Vector3 _ _ z) = z

-- | Ordered pair of magnitudes of the vector
vector3XYZ :: RealFloat a => Vector3 a 
              -> (a, a, a) -- ^ (X, Y, Z)
vector3XYZ (Vector3 x y z) = (x, y, z)

-- | Spherical coordinates to vector
vector3Spherical :: RealFloat a => a -- ^ magnitude
                    -> a -- ^ Theta-direction
                    -> a -- ^ Phi-direction
                    -> Vector3 a
vector3Spherical rho theta phi =
    Vector3 (rhoSinPhi * cos theta) (rhoSinPhi * sin theta) (rho * cos phi)
    where
	rhoSinPhi = rho * sin phi

-- | Magnitude of a vector
vector3Rho :: RealFloat a => Vector3 a -> a
vector3Rho (Vector3 x y z) = sqrt (x * x + y * y + z * z)

-- | Theta-direction of a vector
vector3Theta :: RealFloat a => Vector3 a -> a
vector3Theta (Vector3 x y _) = atan2 y x

-- | Phi-direction of a vector
vector3Phi :: RealFloat a => Vector3 a -> a
vector3Phi v@(Vector3 _ _ z) = acos (z / vector3Rho v)

-- | Magnitude and directions of a vector as an ordered triple
vector3RhoThetaPhi :: RealFloat a => Vector3 a 
                      -> (a, a, a) -- ^ (Rho, Theta, Phi)
vector3RhoThetaPhi (Vector3 x y z) = (rho, theta, phi)
    where
        rho   = sqrt (x * x + y * y + z * z)
        theta = atan2 y x
	phi   = acos (z / rho)

instance RealFloat a => VectorSpace (Vector3 a) a where
    zeroVector = Vector3 0 0 0

    a *^ (Vector3 x y z) = Vector3 (a * x) (a * y) (a * z)

    (Vector3 x y z) ^/ a = Vector3 (x / a) (y / a) (z / a)

    negateVector (Vector3 x y z) = (Vector3 (-x) (-y) (-z))

    (Vector3 x1 y1 z1) ^+^ (Vector3 x2 y2 z2) = Vector3 (x1+x2) (y1+y2) (z1+z2)

    (Vector3 x1 y1 z1) ^-^ (Vector3 x2 y2 z2) = Vector3 (x1-x2) (y1-y2) (z1-z2)

    (Vector3 x1 y1 z1) `dot` (Vector3 x2 y2 z2) = x1 * x2 + y1 * y2 + z1 * z2


-- | Rotate a vector
vector3Rotate :: RealFloat a => 
                 a -- ^ Difference of theta
                 -> a -- ^ Difference of phi 
                 -> Vector3 a -- ^ Initial vector
                 -> Vector3 a -- ^ Rotated vector
vector3Rotate theta' phi' v =
    vector3Spherical (vector3Rho v)
		     (vector3Theta v + theta')
		     (vector3Phi v + phi')

instance RealFloat a => Forceable (Vector3 a) where
     force = id
