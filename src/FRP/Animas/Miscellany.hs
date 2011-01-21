-- |
-- Module      :  FRP.Animas.Miscellany
-- Copyright   :  (c) Antony Courtney and Henrik Nilsson, Yale University, 2003
-- License     :  BSD-style (see the LICENSE file in the distribution)
--
-- Maintainer  :  nilsson@cs.yale.edu
-- Stability   :  provisional
-- Portability :  portable
--
-- Collection of entities that really should be part
-- of the Haskell 98 prelude or simply have no better
-- home.
--
-- !!! Reverse function composition should go.
-- !!! Better to use '<<<' and '>>>' for, respectively,
-- !!! function composition and reverse function composition.

module FRP.Animas.Miscellany (
    ( # ),
    dup,
    swap,
    mapFst,
    mapSnd,
    sel3_1, sel3_2, sel3_3,
    sel4_1, sel4_2, sel4_3, sel4_4,
    sel5_1, sel5_2, sel5_3, sel5_4, sel5_5,
    fDiv,
    fMod,
    fDivMod
) where

infixl 9 #
infixl 7 `fDiv`, `fMod`

-- | Reverse composition
( # ) :: (a -> b) -> (b -> c) -> (a -> c)
(#) = flip (.)

-- | Duplicate a value into a pair
dup :: a -> (a,a)
dup x = (x,x)

-- | Swap the values in a pair
swap :: (a,b) -> (b,a)
swap ~(x,y) = (y,x)

-- | Apply a function to the first value in each pair in a list of pairs.
mapFst :: (a -> b) -> [(a,c)] -> [(b,c)]
mapFst _ []             = []
mapFst f ((x, y) : xys) = (f x, y) : mapFst f xys

-- | Above, but apply the function to the second value
mapSnd :: (a -> b) -> [(c,a)] -> [(c,b)]
mapSnd _ []             = []
mapSnd f ((x, y) : xys) = (x, f y) : mapSnd f xys

-- | First value of a triple
sel3_1 :: (a, b, c) -> a
sel3_1 (x,_,_) = x

-- | Second value of a triple
sel3_2 :: (a, b, c) -> b
sel3_2 (_,x,_) = x

-- | Third value of a triple
sel3_3 :: (a, b, c) -> c
sel3_3 (_,_,x) = x

-- | First value of a quadruple
sel4_1 :: (a, b, c, d) -> a
sel4_1 (x,_,_,_) = x

-- | Second value of a quadruple
sel4_2 :: (a, b, c, d) -> b
sel4_2 (_,x,_,_) = x

-- | Third value of a quadruple
sel4_3 :: (a, b, c, d) -> c
sel4_3 (_,_,x,_) = x

-- | Fourth value of a quadruple
sel4_4 :: (a, b, c, d) -> d
sel4_4 (_,_,_,x) = x

-- | First value of a quintuple
sel5_1 :: (a, b, c, d, e) -> a
sel5_1 (x,_,_,_,_) = x

-- | Second value of a quintuple
sel5_2 :: (a, b, c, d, e) -> b
sel5_2 (_,x,_,_,_) = x

-- | Third value of a quintuple
sel5_3 :: (a, b, c, d, e) -> c
sel5_3 (_,_,x,_,_) = x

-- | Fourth value of a quintuple
sel5_4 :: (a, b, c, d, e) -> d
sel5_4 (_,_,_,x,_) = x

-- | Fifth value of a quintuple
sel5_5 :: (a, b, c, d, e) -> e
sel5_5 (_,_,_,_,x) = x

-- | Whole integer quotient
fDiv :: (RealFrac a) => a -- ^ Dividend
        -> a -- ^ Divisor
        -> Integer -- ^ Integer quotient
fDiv x y = fst $ fDivMod x y

-- | Remainder after whole integer quotient
fMod :: (RealFrac a) => a -- ^ Dividend
        -> a -- ^ Divisor
        -> a -- ^ Remainder
fMod x y = snd $ fDivMod x y

-- | Whole integer quotient and remainder 
fDivMod :: (RealFrac a) => a -- ^ Dividend
           -> a -- ^ Divisor
           -> (Integer, a) -- ^ Integer quotient and remainder
fDivMod x y = (q, r)
    where
        q = (floor (x/y))
        r = x - fromIntegral q * y
