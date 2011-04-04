-- |
-- Module      :  FRP.Animas.Utilities
-- Copyright   :  (c) Antony Courtney and Henrik Nilsson, Yale University, 2003
-- License     :  BSD-style (see the LICENSE file in the distribution)
--
-- Maintainer  :  nilsson@cs.yale.edu
-- Stability   :  provisional
-- Portability :  portable
--
-- Derived utility definitions.
--
-- ToDo:
--
-- * Possibly add
--       impulse :: VectorSpace a k => a -> Event a
--   But to do that, we need access to Event, which we currently do not have.
--
-- * The general arrow utilities should be moved to a module
--   FRP.Animas.Utilities.
--
-- * I'm not sure structuring the Animas \"core\" according to what is
--   core functionality and what's not is all that useful. There are
--   many cases where we want to implement combinators that fairly
--   easily could be implemented in terms of others as primitives simply
--   because we expect that that implementation is going to be much more
--   efficient, and that the combinators are used sufficiently often to
--   warrant doing this. E.g. 'switch' should be a primitive, even though
--   it could be derived from 'pSwitch'.
--
-- * Reconsider 'recur'. If an event source has an immediate occurrence,
--   we'll get into a loop. For example: recur now. Maybe suppress
--   initial occurrences? Initial occurrences are rather pointless in this
--   case anyway.

module FRP.Animas.Utilities (
    arr2,
    arr3,
    arr4,
    arr5,
    lift0,
    lift1,
    lift2,
    lift3,
    lift4,
    lift5,
    snap,
    snapAfter,
    sample,
    recur,
    andThen,
    sampleWindow,
 --   parZ,
 --   pSwitchZ,
 --   dpSwitchZ,
 --   rpSwitchZ,
 --   drpSwitchZ,
    provided,
    old_dHold,
    dTrackAndHold,
    old_accumHold,
    old_dAccumHold,
    old_accumHoldBy,
    old_dAccumHoldBy,
    count,
    fby,
    impulseIntegral,
    old_impulseIntegral
) where

import FRP.Animas.Diagnostics
import FRP.Animas


infixr 5 `andThen`
infixr 0 `fby`

arr2 :: Arrow a => (b -> c -> d) -> a (b, c) d
arr2 = arr . uncurry


arr3 :: Arrow a => (b -> c -> d -> e) -> a (b, c, d) e
arr3 = arr . \h (b, c, d) -> h b c d


arr4 :: Arrow a => (b -> c -> d -> e -> f) -> a (b, c, d, e) f
arr4 = arr . \h (b, c, d, e) -> h b c d e


arr5 :: Arrow a => (b -> c -> d -> e -> f -> g) -> a (b, c, d, e, f) g
arr5 = arr . \h (b, c, d, e, f) -> h b c d e f


lift0 :: Arrow a => c -> a b c
lift0 c = arr (const c)


lift1 :: Arrow a => (c -> d) -> (a b c -> a b d)
lift1 f = \a -> a >>> arr f


lift2 :: Arrow a => (c -> d -> e) -> (a b c -> a b d -> a b e)
lift2 f = \a1 a2 -> a1 &&& a2 >>> arr2 f


lift3 :: Arrow a => (c -> d -> e -> f) -> (a b c -> a b d -> a b e -> a b f)
lift3 f = \a1 a2 a3 -> (lift2 f) a1 a2 &&& a3 >>> arr2 ($)


lift4 :: Arrow a => (c->d->e->f->g) -> (a b c->a b d->a b e->a b f->a b g)
lift4 f = \a1 a2 a3 a4 -> (lift3 f) a1 a2 a3 &&& a4 >>> arr2 ($)


lift5 :: Arrow a =>
    (c->d->e->f->g->h) -> (a b c->a b d->a b e->a b f->a b g->a b h)
lift5 f = \a1 a2 a3 a4 a5 ->(lift4 f) a1 a2 a3 a4 &&& a5 >>> arr2 ($)


-- | Produce an event with the input value at time 0
snap :: SF a (Event a)
snap = switch (never &&& (identity &&& now () >>^ \(a, e) -> e `tag` a)) now


-- | Produce an event with the input value at or as soon after the specified
-- time delay.
snapAfter :: Time -> SF a (Event a)
snapAfter t_ev = switch (never
			 &&& (identity
			      &&& after t_ev () >>^ \(a, e) -> e `tag` a))
			now


-- | Sample a signal at regular intervals.
sample :: Time -> SF a (Event a)
sample p_ev = identity &&& repeatedly p_ev () >>^ \(a, e) -> e `tag` a


-- | Restart an event source directly after its first event occurence
recur :: SF a (Event b) -> SF a (Event b)
recur sfe = switch (never &&& sfe) $ \b -> Event b --> (recur (NoEvent-->sfe))

-- | Start a second event source as soon as the first produces an event.
-- (When used infix, andThen is right associative, so, for instance,
-- x `andThen` y `andThen` z will produce the first event of x, then of y,
-- then of z.
andThen :: SF a (Event b) -> SF a (Event b) -> SF a (Event b)
sfe1 `andThen` sfe2 = dSwitch (sfe1 >>^ dup) (const sfe2)


sampleWindow :: Int -> Time -> SF a (Event [a])
sampleWindow wl q =
    identity &&& afterEachCat (repeat (q, ()))
    >>> arr (\(a, e) -> fmap (map (const a)) e)
    >>> accumBy updateWindow []
    where
        updateWindow w as = drop (max (length w' - wl) 0) w'
            where
	        w' = w ++ as

{-safeZip :: String -> [a] -> [b] -> [(a,b)]
safeZip fn as bs = safeZip' as bs
    where
	safeZip' _  []     = []
	safeZip' as (b:bs) = (head' as, b) : safeZip' (tail' as) bs

	head' []    = err
	head' (a:_) = a

	tail' []     = err
	tail' (_:as) = as

	err = usrErr "AFRPUtilities" fn "Input list too short."
-}
{-
parZ :: [SF a b] -> SF [a] [b]
parZ = par (safeZip "parZ")


pSwitchZ :: [SF a b] -> SF ([a],[b]) (Event c) -> ([SF a b] -> c -> SF [a] [b])
            -> SF [a] [b]
pSwitchZ = pSwitch (safeZip "pSwitchZ")


dpSwitchZ :: [SF a b] -> SF ([a],[b]) (Event c) -> ([SF a b] -> c ->SF [a] [b])
             -> SF [a] [b]
dpSwitchZ = dpSwitch (safeZip "dpSwitchZ")


rpSwitchZ :: [SF a b] -> SF ([a], Event ([SF a b] -> [SF a b])) [b]
rpSwitchZ = rpSwitch (safeZip "rpSwitchZ")


drpSwitchZ :: [SF a b] -> SF ([a], Event ([SF a b] -> [SF a b])) [b]
drpSwitchZ = drpSwitch (safeZip "drpSwitchZ")
-}

-- | Run one SF if a predicate is true, otherwise run another SF.
provided :: (a -> Bool) -- ^ Predicate on input values
            -> SF a b -- ^ SF if predicate is true
            -> SF a b -- ^ SF if predicate is false
            -> SF a b -- ^ SF total
provided p sft sff =
    switch (constant undefined &&& snap) $ \a0 ->
    if p a0 then stt else stf
    where
	stt = switch (sft &&& (not . p ^>> edge)) (const stf)
        stf = switch (sff &&& (p ^>> edge)) (const stt)

old_dHold :: a -> SF (Event a) a
old_dHold a0 = dSwitch (constant a0 &&& identity) dHold'
    where
	dHold' a = dSwitch (constant a &&& notYet) dHold'


-- | Decoupled track and hold: on occurence of a 'Just' input,
-- the /next/ output is the value of the 'Just' value.
dTrackAndHold :: a -> SF (Maybe a) a
dTrackAndHold a_init = trackAndHold a_init >>> iPre a_init

old_accumHold :: a -> SF (Event (a -> a)) a
old_accumHold a_init = old_accum a_init >>> old_hold a_init

old_dAccumHold :: a -> SF (Event (a -> a)) a
old_dAccumHold a_init = old_accum a_init >>> old_dHold a_init

old_accumHoldBy :: (b -> a -> b) -> b -> SF (Event a) b
old_accumHoldBy f b_init = old_accumBy f b_init >>> old_hold b_init

old_dAccumHoldBy :: (b -> a -> b) -> b -> SF (Event a) b
old_dAccumHoldBy f b_init = old_accumBy f b_init >>> old_dHold b_init

-- | Count the number of event occurences, producing a new event
-- occurence with each updated count.
count :: Integral b => SF (Event a) (Event b)
count = accumBy (\n _ -> n + 1) 0

fby :: b -> SF a b -> SF a b
b0 `fby` sf = b0 --> sf >>> pre

impulseIntegral :: VectorSpace a k => SF (a, Event a) a
impulseIntegral = (integral *** accumHoldBy (^+^) zeroVector) >>^ uncurry (^+^)

old_impulseIntegral :: VectorSpace a k => SF (a, Event a) a
old_impulseIntegral = (integral *** old_accumHoldBy (^+^) zeroVector) >>^ uncurry (^+^)
