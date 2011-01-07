{-# LANGUAGE GADTs, Rank2Types, CPP #-}
-- |
-- Module      :  FRP.Yampa
-- Copyright   :  (c) Antony Courtney and Henrik Nilsson, Yale University, 2003.
--                Modifications by Edward Amsden and Matthew Hayden
-- License     :  BSD-style (see the LICENSE file in the distribution)
--
-- Maintainer  :  edwardamsden@gmail.com
-- Stability   :  provisional
-- Portability :  non-portable (GHC extensions)

module FRP.Yampa (
    -- * Re-exported modules 
    module Control.Arrow,
    module FRP.Yampa.VectorSpace,
    -- * Random-number classes  
    RandomGen(..),
    Random(..),
    -- * Convenience operators
    ( # ),
    dup,
    swap,
    -- * Datatypes
    Time,
    SF,	
    Event(..),
    -- * Pure signal functions
    arrPrim, arrEPrim,
    identity,
    constant,
    -- * Time signal functions
    localTime,
    time,
    -- * Initialization
    -- | These operators provide means of specifying the initial
    -- input or output of a signal function, overriding the signal function for
    -- the first cycle of animation
    (-->),
    (>--),
    (-=>),
    (>=-),
    initially,
    -- * Accumulator-based signal functions
    sscan,
    sscanPrim,
    -- * Events
    -- ** Basic event producers 
    never,
    now,
    after,
    repeatedly,
    afterEach,
    afterEachCat,
    edge,
    iEdge,
    edgeTag,
    edgeJust,
    edgeBy,
    once,
    noEvent,
    noEventFst,
    noEventSnd,
    -- ** Event manipulation
    delayEvent,
    delayEventCat,
    takeEvents,
    dropEvents,
    notYet,
    -- ** Stateful event processing
    old_hold,
    hold,
    dHold,
    trackAndHold,
    old_accum,
    old_accumBy,
    old_accumFilter,
    accum,
    accumHold,
    dAccumHold,
    accumBy,
    accumHoldBy,
    dAccumHoldBy,
    accumFilter,
    -- * Unlifted event functions
    event,
    fromEvent,
    isEvent,
    isNoEvent,
    tag,
    tagWith,
    attach,
    lMerge,
    rMerge,
    merge,
    mergeBy,
    mapMerge,
    mergeEvents,
    catEvents,
    joinE,
    splitE,
    filterE,
    mapFilterE,
    gate,
    -- * Switches
    -- | Switches provide run-time modification of the signal network. 
    -- Most switching combinators provided two varieties: an
    -- \"instantaneous\" version and a \"decoupled version\". The difference
    -- lies in which signal function is used to produce the value at the instant
    -- of switching. For an instantaneous switch, the signal function being 
    -- switched in is used to produce the value. For a decoupled switch, that
    -- signal function is used to produce the value at the /next/ instant,
    -- while the signal function being switched out is still used to produce
    -- the value at the instant of switching. This is useful for (among other
    -- things) ensuring that looped signal functions are well-founded
    -- recursively. Decoupled varieties of switches are prefixed with a \"d\".
    
    -- ** Event-based switches
    switch,  dSwitch,	    
    rSwitch, drSwitch,	
    kSwitch, dkSwitch,
    -- ** Parallel switches (collections of signal functions)
    parB,		
    pSwitchB,dpSwitchB, 
    rpSwitchB,drpSwitchB,
    par,
    pSwitch, dpSwitch,
    rpSwitch,drpSwitch, 
    -- * Delays
    old_pre, old_iPre,
    pre,
    iPre,
    delay,
    -- * Calculus
    integral,
    derivative,
    imIntegral,
    -- * Looping
    -- | See also the 'loop' combinator from the 'ArrowLoop' class.
    loopPre,
    loopIntegral,
    -- * Randomized signal functions
    noise,
    noiseR,
    occasionally,
    -- * Animation
    ReactHandle,
    reactimate,
    reactInit,
    react,
    embed,
    embedSynch,
    deltaEncode,
    deltaEncodeBy
) where

import Control.Monad (unless)
import System.Random (RandomGen(..), Random(..))

#if __GLASGOW_HASKELL__ >= 610
import qualified Control.Category (Category(..))
#else
#endif

import Control.Arrow
import FRP.Yampa.Diagnostics
import FRP.Yampa.Miscellany (( # ), dup, swap)
import FRP.Yampa.Event
import FRP.Yampa.VectorSpace

import Data.IORef

infixr 0 -->, >--, -=>, >=-

-- Time/DTime should be parameterized with a Num class restriction
-- | Time representation for signal functions
type Time = Double

type DTime = Double

-- | A signal function
data SF a b = SF {sfTF :: a -> Transition a b}

data SF' a b where
    SFArr   :: !(DTime -> a -> Transition a b) -> !(FunDesc a b) -> SF' a b
    SFSScan :: !(DTime -> a -> Transition a b)
               -> !(c -> a -> Maybe (c, b)) -> !c -> b 
               -> SF' a b
    SFEP   :: !(DTime -> Event a -> Transition (Event a) b)
              -> !(c -> a -> (c, b, b)) -> !c -> b
              -> SF' (Event a) b
    SFCpAXA :: !(DTime -> a -> Transition a d)
               -> !(FunDesc a b) -> !(SF' b c) -> !(FunDesc c d)
               -> SF' a d
    SF' :: !(DTime -> a -> Transition a b) -> SF' a b
type Transition a b = (SF' a b, b)


sfTF' :: SF' a b -> (DTime -> a -> Transition a b)
sfTF' (SFArr tf _)       = tf
sfTF' (SFSScan tf _ _ _) = tf
sfTF' (SFEP tf _ _ _)    = tf
sfTF' (SFCpAXA tf _ _ _) = tf
sfTF' (SF' tf)           = tf

sfArr :: FunDesc a b -> SF' a b
sfArr FDI         = sfId
sfArr (FDC b)     = sfConst b
sfArr (FDE f fne) = sfArrE f fne
sfArr (FDG f)     = sfArrG f

sfId :: SF' a a
sfId = sf
    where
	sf = SFArr (\_ a -> (sf, a)) FDI


sfConst :: b -> SF' a b
sfConst b = sf
    where
	sf = SFArr (\_ _ -> (sf, b)) (FDC b)


sfNever :: SF' a (Event b)
sfNever = sfConst NoEvent

sfArrE :: (Event a -> b) -> b -> SF' (Event a) b
sfArrE f fne = sf
    where
        sf  = SFArr (\_ ea -> (sf, case ea of NoEvent -> fne ; _ -> f ea))
                    (FDE f fne)

sfArrG :: (a -> b) -> SF' a b
sfArrG f = sf
    where
	sf = SFArr (\_ a -> (sf, f a)) (FDG f)


sfSScan :: (c -> a -> Maybe (c, b)) -> c -> b -> SF' a b
sfSScan f c b = sf 
    where
        sf = SFSScan tf f c b
	tf _ a = case f c a of
		     Nothing       -> (sf, b)
		     Just (c', b') -> (sfSScan f c' b', b')

sscanPrim :: (c -> a -> Maybe (c, b)) -> c -> b -> SF a b
sscanPrim f c_init b_init = SF {sfTF = tf0}
    where
        tf0 a0 = case f c_init a0 of
                     Nothing       -> (sfSScan f c_init b_init, b_init)
	             Just (c', b') -> (sfSScan f c' b', b')

sfEP :: (c -> a -> (c, b, b)) -> c -> b -> SF' (Event a) b
sfEP f c bne = sf
    where
        sf = SFEP (\_ ea -> case ea of
                                 NoEvent -> (sf, bne)
                                 Event a -> let
                                                (c', b, bne') = f c a
                                            in
                                                (sfEP f c' bne', b))
                  f
                  c
                  bne

epPrim :: (c -> a -> (c, b, b)) -> c -> b -> SF (Event a) b
epPrim f c bne = SF {sfTF = tf0}
    where
        tf0 NoEvent   = (sfEP f c bne, bne)
        tf0 (Event a) = let
                            (c', b, bne') = f c a
                        in
                            (sfEP f c' bne', b)

data FunDesc a b where
    FDI :: FunDesc a a
    FDC :: b -> FunDesc a b
    FDE :: (Event a -> b) -> b -> FunDesc (Event a) b
    FDG :: (a -> b) -> FunDesc a b

fdFun :: FunDesc a b -> (a -> b)
fdFun FDI       = id
fdFun (FDC b)   = const b
fdFun (FDE f _) = f
fdFun (FDG f)   = f

fdComp :: FunDesc a b -> FunDesc b c -> FunDesc a c
fdComp FDI           fd2     = fd2
fdComp fd1           FDI     = fd1
fdComp (FDC b)       fd2     = FDC ((fdFun fd2) b)
fdComp _             (FDC c) = FDC c

fdComp (FDE f1 f1ne) fd2 = FDE (f2 . f1) (f2 f1ne)
    where
        f2 = fdFun fd2
fdComp (FDG f1) (FDE f2 f2ne) = FDG f
    where
        f a = case f1 a of
                  NoEvent -> f2ne
                  f1a     -> f2 f1a
fdComp (FDG f1) fd2 = FDG (fdFun fd2 . f1)


fdPar :: FunDesc a b -> FunDesc c d -> FunDesc (a,c) (b,d)
fdPar FDI     FDI     = FDI
fdPar FDI     (FDC d) = FDG (\(~(a, _)) -> (a, d))
fdPar FDI     fd2     = FDG (\(~(a, c)) -> (a, (fdFun fd2) c))
fdPar (FDC b) FDI     = FDG (\(~(_, c)) -> (b, c))
fdPar (FDC b) (FDC d) = FDC (b, d)
fdPar (FDC b) fd2     = FDG (\(~(_, c)) -> (b, (fdFun fd2) c))
fdPar fd1     fd2     = FDG (\(~(a, c)) -> ((fdFun fd1) a, (fdFun fd2) c))


fdFanOut :: FunDesc a b -> FunDesc a c -> FunDesc a (b,c)
fdFanOut FDI     FDI     = FDG dup
fdFanOut FDI     (FDC c) = FDG (\a -> (a, c))
fdFanOut FDI     fd2     = FDG (\a -> (a, (fdFun fd2) a))
fdFanOut (FDC b) FDI     = FDG (\a -> (b, a))
fdFanOut (FDC b) (FDC c) = FDC (b, c)
fdFanOut (FDC b) fd2     = FDG (\a -> (b, (fdFun fd2) a))
fdFanOut (FDE f1 f1ne) (FDE f2 f2ne) = FDE f1f2 f1f2ne
    where
       f1f2 NoEvent      = f1f2ne
       f1f2 ea@(Event _) = (f1 ea, f2 ea)

       f1f2ne = (f1ne, f2ne)
fdFanOut fd1 fd2 =
    FDG (\a -> ((fdFun fd1) a, (fdFun fd2) a))

vfyNoEv :: Event a -> b -> b
vfyNoEv NoEvent b = b
vfyNoEv _       _  = usrErr "AFRP" "vfyNoEv" "Assertion failed: Functions on events must not map NoEvent to Event."

freeze :: SF' a b -> DTime -> SF a b
freeze sf dt = SF {sfTF = (sfTF' sf) dt}

freezeCol :: Functor col => col (SF' a b) -> DTime -> col (SF a b)
freezeCol sfs dt = fmap (flip freeze dt) sfs

#if __GLASGOW_HASKELL__ >= 610
instance Control.Category.Category SF where
     (.) = flip compPrim 
     id = SF $ \x -> (sfId,x)
#else
#endif

instance Arrow SF where
    arr    = arrPrim
    first  = firstPrim
    second = secondPrim
    (***)  = parSplitPrim
    (&&&)  = parFanOutPrim
#if __GLASGOW_HASKELL__ >= 610
#else
    (>>>)  = compPrim
#endif

-- | Lifts a function to a pure signal function. Use 'arr' from the 'Arrow'
--   class, rather than this function.
{-# NOINLINE arrPrim #-}
arrPrim :: (a -> b) -> SF a b
arrPrim f = SF {sfTF = \a -> (sfArrG f, f a)}


{-# RULES "arrPrim/arrEPrim" arrPrim = arrEPrim #-}
-- | Lifts a function with an event input to a pure signal function
-- on events. Use 'arr' from the 'Arrow' class, rather than this function.
arrEPrim :: (Event a -> b) -> SF (Event a) b
arrEPrim f = SF {sfTF = \a -> (sfArrE f (f NoEvent), f a)}

compPrim :: SF a b -> SF b c -> SF a c
compPrim (SF {sfTF = tf10}) (SF {sfTF = tf20}) = SF {sfTF = tf0}
    where
	tf0 a0 = (cpXX sf1 sf2, c0)
	    where
		(sf1, b0) = tf10 a0
		(sf2, c0) = tf20 b0

cpXX :: SF' a b -> SF' b c -> SF' a c
cpXX (SFArr _ fd1)       sf2               = cpAX fd1 sf2
cpXX sf1                 (SFArr _ fd2)     = cpXA sf1 fd2
cpXX (SFSScan _ f1 s1 b) (SFSScan _ f2 s2 c) =
    sfSScan f (s1, b, s2, c) c
    where
        f (s1, b, s2, c) a =
            let
                (u, s1',  b') = case f1 s1 a of
                                    Nothing       -> (True, s1, b)
                                    Just (s1',b') -> (False,  s1', b')
            in
                case f2 s2 b' of
                    Nothing | u         -> Nothing
                            | otherwise -> Just ((s1', b', s2, c), c)
                    Just (s2', c') -> Just ((s1', b', s2', c'), c')
cpXX (SFSScan _ f1 s1 eb) (SFEP _ f2 s2 cne) =
    sfSScan f (s1, eb, s2, cne) cne
    where
        f (s1, eb, s2, cne) a =
            case f1 s1 a of
                Nothing ->
                    case eb of
                        NoEvent -> Nothing
                        Event b ->
                            let (s2', c, cne') = f2 s2 b
                            in
                                Just ((s1, eb, s2', cne'), c)
                Just (s1', eb') ->
                    case eb' of
                        NoEvent -> Just ((s1', eb', s2, cne), cne)
                        Event b ->
                            let (s2', c, cne') = f2 s2 b
                            in
                                Just ((s1', eb', s2', cne'), c)
cpXX (SFEP _ f1 s1 bne) (SFSScan _ f2 s2 c) =
    sfSScan f (s1, bne, s2, c) c
    where
        f (s1, bne, s2, c) ea =
            let (u, s1', b', bne') = case ea of
                                         NoEvent -> (True, s1, bne, bne)
                                         Event a ->
                                             let (s1', b, bne') = f1 s1 a
                                             in
                                                  (False, s1', b, bne')
            in
                case f2 s2 b' of
                    Nothing | u         -> Nothing
                            | otherwise -> Just (seq s1' (s1', bne', s2, c), c)
                    Just (s2', c') -> Just (seq s1' (s1', bne', s2', c'), c')
cpXX (SFEP _ f1 s1 bne) (SFEP _ f2 s2 cne) =
    sfEP f (s1, s2, cne) (vfyNoEv bne cne)
    where
	f (s1, s2, cne) a =
	    case f1 s1 a of
		(s1', NoEvent, NoEvent) -> ((s1', s2, cne), cne, cne)
		(s1', Event b, NoEvent) ->
		    let (s2', c, cne') = f2 s2 b in ((s1', s2', cne'), c, cne')
                _ -> usrErr "AFRP" "cpXX" "Assertion failed: Functions on events must not map NoEvent to Event."
cpXX sf1@(SFEP _ _ _ _) (SFCpAXA _ (FDE f21 f21ne) sf22 fd23) =
    cpXX (cpXE sf1 f21 f21ne) (cpXA sf22 fd23)
cpXX sf1@(SFEP _ _ _ _) (SFCpAXA _ (FDG f21) sf22 fd23) =
    cpXX (cpXG sf1 f21) (cpXA sf22 fd23)
cpXX (SFCpAXA _ fd11 sf12 (FDE f13 f13ne)) sf2@(SFEP _ _ _ _) =
    cpXX (cpAX fd11 sf12) (cpEX f13 f13ne sf2) 
cpXX (SFCpAXA _ fd11 sf12 fd13) (SFCpAXA _ fd21 sf22 fd23) =
    cpAXA fd11 (cpXX (cpXA sf12 (fdComp fd13 fd21)) sf22) fd23
cpXX sf1 sf2 = SF' tf    
  where
        tf dt a = (cpXX sf1' sf2', c)
	    where
	        (sf1', b) = (sfTF' sf1) dt a
		(sf2', c) = (sfTF' sf2) dt b

cpAXA :: FunDesc a b -> SF' b c -> FunDesc c d -> SF' a d
cpAXA FDI     sf2 fd3     = cpXA sf2 fd3
cpAXA fd1     sf2 FDI     = cpAX fd1 sf2
cpAXA (FDC b) sf2 fd3     = cpCXA b sf2 fd3
cpAXA _       _   (FDC d) = sfConst d        
cpAXA fd1     sf2 fd3     = 
    cpAXAAux fd1 (fdFun fd1) fd3 (fdFun fd3) sf2
    where
        cpAXAAux :: FunDesc a b -> (a -> b) -> FunDesc c d -> (c -> d)
                    -> SF' b c -> SF' a d
        cpAXAAux fd1 _ fd3 _ (SFArr _ fd2) =
            sfArr (fdComp (fdComp fd1 fd2) fd3)
        cpAXAAux fd1 _ fd3 _ sf2@(SFSScan _ _ _ _) =
            cpAX fd1 (cpXA sf2 fd3)
        cpAXAAux fd1 _ fd3 _ sf2@(SFEP _ _ _ _) =
            cpAX fd1 (cpXA sf2 fd3)
        cpAXAAux fd1 _ fd3 _ (SFCpAXA _ fd21 sf22 fd23) =
            cpAXA (fdComp fd1 fd21) sf22 (fdComp fd23 fd3)
        cpAXAAux fd1 f1 fd3 f3 sf2 = SFCpAXA tf fd1 sf2 fd3
	    where
		tf dt a = (cpAXAAux fd1 f1 fd3 f3 sf2', f3 c)
		    where
			(sf2', c) = (sfTF' sf2) dt (f1 a)

cpAX :: FunDesc a b -> SF' b c -> SF' a c
cpAX FDI           sf2 = sf2
cpAX (FDC b)       sf2 = cpCX b sf2
cpAX (FDE f1 f1ne) sf2 = cpEX f1 f1ne sf2
cpAX (FDG f1)      sf2 = cpGX f1 sf2

cpXA :: SF' a b -> FunDesc b c -> SF' a c
cpXA sf1 FDI           = sf1
cpXA _   (FDC c)       = sfConst c
cpXA sf1 (FDE f2 f2ne) = cpXE sf1 f2 f2ne
cpXA sf1 (FDG f2)      = cpXG sf1 f2

cpCX :: b -> SF' b c -> SF' a c
cpCX b (SFArr _ fd2) = sfConst ((fdFun fd2) b)
cpCX b (SFSScan _ f s c) = sfSScan (\s _ -> f s b) s c
cpCX b (SFEP _ _ _ cne) = sfConst (vfyNoEv b cne)
cpCX b (SFCpAXA _ fd21 sf22 fd23) =
    cpCXA ((fdFun fd21) b) sf22 fd23
cpCX b sf2 = SFCpAXA tf (FDC b) sf2 FDI
    where
	tf dt _ = (cpCX b sf2', c)
	    where
		(sf2', c) = (sfTF' sf2) dt b

cpCXA :: b -> SF' b c -> FunDesc c d -> SF' a d
cpCXA b sf2 FDI     = cpCX b sf2
cpCXA _ _   (FDC c) = sfConst c
cpCXA b sf2 fd3     = cpCXAAux (FDC b) b fd3 (fdFun fd3) sf2
    where
        cpCXAAux :: FunDesc a b -> b -> FunDesc c d -> (c -> d)
                    -> SF' b c -> SF' a d
        cpCXAAux _ b _ f3 (SFArr _ fd2)     = sfConst (f3 ((fdFun fd2) b))
        cpCXAAux _ b _ f3 (SFSScan _ f s c) = sfSScan f' s (f3 c)
            where
	        f' s _ = case f s b of
                             Nothing -> Nothing
                             Just (s', c') -> Just (s', f3 c') 
        cpCXAAux _ b _   f3 (SFEP _ _ _ cne) = sfConst (f3 (vfyNoEv b cne))
        cpCXAAux _ b fd3 _  (SFCpAXA _ fd21 sf22 fd23) =
	    cpCXA ((fdFun fd21) b) sf22 (fdComp fd23 fd3)
	cpCXAAux fd1 b fd3 f3 sf2 = SFCpAXA tf fd1 sf2 fd3
	    where
		tf dt _ = (cpCXAAux fd1 b fd3 f3 sf2', f3 c)
		    where
			(sf2', c) = (sfTF' sf2) dt b

cpGX :: (a -> b) -> SF' b c -> SF' a c
cpGX f1 sf2 = cpGXAux (FDG f1) f1 sf2
    where
	cpGXAux :: FunDesc a b -> (a -> b) -> SF' b c -> SF' a c
	cpGXAux fd1 _ (SFArr _ fd2) = sfArr (fdComp fd1 fd2)
        cpGXAux _ f1 (SFSScan _ f s c) = sfSScan (\s a -> f s (f1 a)) s c
	cpGXAux fd1 _ (SFCpAXA _ fd21 sf22 fd23) =
	    cpAXA (fdComp fd1 fd21) sf22 fd23
	cpGXAux fd1 f1 sf2 = SFCpAXA tf fd1 sf2 FDI
	    where
		tf dt a = (cpGXAux fd1 f1 sf2', c)
		    where
			(sf2', c) = (sfTF' sf2) dt (f1 a)

cpXG :: SF' a b -> (b -> c) -> SF' a c
cpXG sf1 f2 = cpXGAux (FDG f2) f2 sf1
    where
	cpXGAux :: FunDesc b c -> (b -> c) -> SF' a b -> SF' a c
	cpXGAux fd2 _ (SFArr _ fd1) = sfArr (fdComp fd1 fd2)
        cpXGAux _ f2 (SFSScan _ f s b) = sfSScan f' s (f2 b)
            where
	        f' s a = case f s a of
                             Nothing -> Nothing
                             Just (s', b') -> Just (s', f2 b') 
        cpXGAux _ f2 (SFEP _ f1 s bne) = sfEP f s (f2 bne)
            where
                f s a = let (s', b, bne') = f1 s a in (s', f2 b, f2 bne')
	cpXGAux fd2 _ (SFCpAXA _ fd11 sf12 fd22) =
            cpAXA fd11 sf12 (fdComp fd22 fd2)
	cpXGAux fd2 f2 sf1 = SFCpAXA tf FDI sf1 fd2
	    where
		tf dt a = (cpXGAux fd2 f2 sf1', f2 b)
		    where
			(sf1', b) = (sfTF' sf1) dt a
cpEX :: (Event a -> b) -> b -> SF' b c -> SF' (Event a) c
cpEX f1 f1ne sf2 = cpEXAux (FDE f1 f1ne) f1 f1ne sf2
    where
	cpEXAux :: FunDesc (Event a) b -> (Event a -> b) -> b 
                   -> SF' b c -> SF' (Event a) c
	cpEXAux fd1 _ _ (SFArr _ fd2) = sfArr (fdComp fd1 fd2)
        cpEXAux _ f1 _   (SFSScan _ f s c) = sfSScan (\s a -> f s (f1 a)) s c
	cpEXAux _ f1 f1ne (SFEP _ f2 s cne) =
	    sfEP f (s, cne) (vfyNoEv f1ne cne)
            where
                f scne@(s, cne) a =
                    case (f1 (Event a)) of
                        NoEvent -> (scne, cne, cne)
                        Event b ->
                            let (s', c, cne') = f2 s b in ((s', cne'), c, cne')
	cpEXAux fd1 _ _ (SFCpAXA _ fd21 sf22 fd23) =
            cpAXA (fdComp fd1 fd21) sf22 fd23
	cpEXAux fd1 f1 f1ne sf2 = SFCpAXA tf fd1 sf2 FDI
	    where
		tf dt ea = (cpEXAux fd1 f1 f1ne sf2', c)
		    where
                        (sf2', c) =
			    case ea of
				NoEvent -> (sfTF' sf2) dt f1ne
				_       -> (sfTF' sf2) dt (f1 ea)

cpXE :: SF' a (Event b) -> (Event b -> c) -> c -> SF' a c
cpXE sf1 f2 f2ne = cpXEAux (FDE f2 f2ne) f2 f2ne sf1
    where
	cpXEAux :: FunDesc (Event b) c -> (Event b -> c) -> c
		   -> SF' a (Event b) -> SF' a c
        cpXEAux fd2 _ _ (SFArr _ fd1) = sfArr (fdComp fd1 fd2)
        cpXEAux _ f2 f2ne (SFSScan _ f s eb) = sfSScan f' s (f2 eb)
            where
	        f' s a = case f s a of
                             Nothing -> Nothing
                             Just (s', NoEvent) -> Just (s', f2ne) 
                             Just (s', eb')     -> Just (s', f2 eb') 
        cpXEAux _ f2 f2ne (SFEP _ f1 s ebne) =
	    sfEP f s (vfyNoEv ebne f2ne)
            where
                f s a =
                    case f1 s a of
                        (s', NoEvent, NoEvent) -> (s', f2ne,  f2ne)
                        (s', eb,      NoEvent) -> (s', f2 eb, f2ne)
		        _ -> usrErr "AFRP" "cpXEAux" "Assertion failed: Functions on events must not map NoEvent to Event."
        cpXEAux fd2 _ _ (SFCpAXA _ fd11 sf12 fd13) =
            cpAXA fd11 sf12 (fdComp fd13 fd2)
	cpXEAux fd2 f2 f2ne sf1 = SFCpAXA tf FDI sf1 fd2
	    where
		tf dt a = (cpXEAux fd2 f2 f2ne sf1',
                           case eb of NoEvent -> f2ne; _ -> f2 eb)
		    where
                        (sf1', eb) = (sfTF' sf1) dt a

firstPrim :: SF a b -> SF (a,c) (b,c)
firstPrim (SF {sfTF = tf10}) = SF {sfTF = tf0}
    where
        tf0 ~(a0, c0) = (fpAux sf1, (b0, c0))
	    where
		(sf1, b0) = tf10 a0 

fpAux :: SF' a b -> SF' (a,c) (b,c)
fpAux (SFArr _ FDI)       = sfId
fpAux (SFArr _ (FDC b))   = sfArrG (\(~(_, c)) -> (b, c))
fpAux (SFArr _ fd1)       = sfArrG (\(~(a, c)) -> ((fdFun fd1) a, c))
fpAux sf1 = SF' tf
    where
        tf dt ~(a, c) = (fpAux sf1', (b, c))
	    where
		(sf1', b) = (sfTF' sf1) dt a 

secondPrim :: SF a b -> SF (c,a) (c,b)
secondPrim (SF {sfTF = tf10}) = SF {sfTF = tf0}
    where
        tf0 ~(c0, a0) = (spAux sf1, (c0, b0))
	    where
		(sf1, b0) = tf10 a0 

spAux :: SF' a b -> SF' (c,a) (c,b)
spAux (SFArr _ FDI)       = sfId
spAux (SFArr _ (FDC b))   = sfArrG (\(~(c, _)) -> (c, b))
spAux (SFArr _ fd1)       = sfArrG (\(~(c, a)) -> (c, (fdFun fd1) a))
spAux sf1 = SF' tf
    where
        tf dt ~(c, a) = (spAux sf1', (c, b))
	    where
		(sf1', b) = (sfTF' sf1) dt a 

parSplitPrim :: SF a b -> SF c d  -> SF (a,c) (b,d)
parSplitPrim (SF {sfTF = tf10}) (SF {sfTF = tf20}) = SF {sfTF = tf0}
    where
	tf0 ~(a0, c0) = (psXX sf1 sf2, (b0, d0))
	    where
		(sf1, b0) = tf10 a0 
		(sf2, d0) = tf20 c0 

        psXX :: SF' a b -> SF' c d -> SF' (a,c) (b,d)
        psXX (SFArr _ fd1)       (SFArr _ fd2)       = sfArr (fdPar fd1 fd2)
        psXX (SFArr _ FDI)       sf2                 = spAux sf2
	psXX (SFArr _ (FDC b))   sf2                 = psCX b sf2
	psXX (SFArr _ fd1)       sf2                 = psAX (fdFun fd1) sf2
        psXX sf1                 (SFArr _ FDI)       = fpAux sf1
	psXX sf1                 (SFArr _ (FDC d))   = psXC sf1 d
	psXX sf1                 (SFArr _ fd2)       = psXA sf1 (fdFun fd2)
	psXX sf1 sf2 = SF' tf
	    where
		tf dt ~(a, c) = (psXX sf1' sf2', (b, d))
		    where
		        (sf1', b) = (sfTF' sf1) dt a
			(sf2', d) = (sfTF' sf2) dt c
        
        psCX :: b -> SF' c d -> SF' (a,c) (b,d)
	psCX b (SFArr _ fd2)       = sfArr (fdPar (FDC b) fd2)
	psCX b sf2                 = SF' tf
	    where
		tf dt ~(_, c) = (psCX b sf2', (b, d))
		    where
			(sf2', d) = (sfTF' sf2) dt c
        
        psXC :: SF' a b -> d -> SF' (a,c) (b,d)
        psXC (SFArr _ fd1)       d = sfArr (fdPar fd1 (FDC d))
	psXC sf1                 d = SF' tf
	    where
		tf dt ~(a, _) = (psXC sf1' d, (b, d))
		    where
			(sf1', b) = (sfTF' sf1) dt a

        psAX :: (a -> b) -> SF' c d -> SF' (a,c) (b,d)
	psAX f1 (SFArr _ fd2)       = sfArr (fdPar (FDG f1) fd2)
	psAX f1 sf2                 = SF' tf
	    where
		tf dt ~(a, c) = (psAX f1 sf2', (f1 a, d))
		    where
			(sf2', d) = (sfTF' sf2) dt c

        psXA :: SF' a b -> (c -> d) -> SF' (a,c) (b,d)
	psXA (SFArr _ fd1)       f2 = sfArr (fdPar fd1 (FDG f2))
	psXA sf1                 f2 = SF' tf
	    where
		tf dt ~(a, c) = (psXA sf1' f2, (b, f2 c))
		    where
			(sf1', b) = (sfTF' sf1) dt a

parFanOutPrim :: SF a b -> SF a c -> SF a (b, c)
parFanOutPrim (SF {sfTF = tf10}) (SF {sfTF = tf20}) = SF {sfTF = tf0}
    where
	tf0 a0 = (pfoXX sf1 sf2, (b0, c0))
	    where
		(sf1, b0) = tf10 a0 
		(sf2, c0) = tf20 a0 
        pfoXX :: SF' a b -> SF' a c -> SF' a (b ,c)
        pfoXX (SFArr _ fd1)       (SFArr _ fd2)       = sfArr(fdFanOut fd1 fd2)
        pfoXX (SFArr _ FDI)       sf2                 = pfoIX sf2
	pfoXX (SFArr _ (FDC b))   sf2                 = pfoCX b sf2
	pfoXX (SFArr _ fd1)       sf2                 = pfoAX (fdFun fd1) sf2
        pfoXX sf1                 (SFArr _ FDI)       = pfoXI sf1
	pfoXX sf1                 (SFArr _ (FDC c))   = pfoXC sf1 c
	pfoXX sf1                 (SFArr _ fd2)       = pfoXA sf1 (fdFun fd2)
	pfoXX sf1 sf2 = SF' tf
	    where
		tf dt a = (pfoXX sf1' sf2', (b, c))
		    where
		        (sf1', b) = (sfTF' sf1) dt a
			(sf2', c) = (sfTF' sf2) dt a
        pfoIX :: SF' a c -> SF' a (a ,c)
	pfoIX (SFArr _ fd2) = sfArr (fdFanOut FDI fd2)
	pfoIX sf2 = SF' tf
	    where
		tf dt a = (pfoIX sf2', (a, c))
		    where
			(sf2', c) = (sfTF' sf2) dt a
        pfoXI :: SF' a b -> SF' a (b ,a)
	pfoXI (SFArr _ fd1) = sfArr (fdFanOut fd1 FDI)
	pfoXI sf1 = SF' tf
	    where
		tf dt a = (pfoXI sf1', (b, a))
		    where
			(sf1', b) = (sfTF' sf1) dt a
        pfoCX :: b -> SF' a c -> SF' a (b ,c)
        pfoCX b (SFArr _ fd2) = sfArr (fdFanOut (FDC b) fd2)
	pfoCX b sf2 = SF' tf
	    where
		tf dt a = (pfoCX b sf2', (b, c))
		    where
			(sf2', c) = (sfTF' sf2) dt a
        pfoXC :: SF' a b -> c -> SF' a (b ,c)
	pfoXC (SFArr _ fd1) c = sfArr (fdFanOut fd1 (FDC c))
	pfoXC sf1 c = SF' tf
	    where
		tf dt a = (pfoXC sf1' c, (b, c))
		    where
			(sf1', b) = (sfTF' sf1) dt a
        pfoAX :: (a -> b) -> SF' a c -> SF' a (b ,c)
	pfoAX f1 (SFArr _ fd2) = sfArr (fdFanOut (FDG f1) fd2)
	pfoAX f1 sf2 = SF' tf
	    where
		tf dt a = (pfoAX f1 sf2', (f1 a, c))
		    where
			(sf2', c) = (sfTF' sf2) dt a
        pfoXA :: SF' a b -> (a -> c) -> SF' a (b ,c)
	pfoXA (SFArr _ fd1) f2 = sfArr (fdFanOut fd1 (FDG f2))
	pfoXA sf1 f2 = SF' tf
	    where
		tf dt a = (pfoXA sf1' f2, (b, f2 a))
		    where
			(sf1', b) = (sfTF' sf1) dt a

instance ArrowLoop SF where
    loop = loopPrim

-- | Loop a signal function.
-- Use the 'loop' function from the 'ArrowLoop' class,
-- rather than this function. 
-- The second output is connected to the second input. This permits recursion 
-- by making the output of a signal function available to itself. 
loopPrim :: SF (a,c) (b,c) -- ^ Signal function, producing output as which 
                           -- it will receive as input.
            -> SF a b -- ^ Looped signal function
loopPrim (SF {sfTF = tf10}) = SF {sfTF = tf0}
    where
	tf0 a0 = (loopAux sf1, b0)
	    where
	        (sf1, (b0, c0)) = tf10 (a0, c0)

        loopAux :: SF' (a,c) (b,c) -> SF' a b
	loopAux (SFArr _ FDI) = sfId
        loopAux (SFArr _ (FDC (b, _))) = sfConst b
	loopAux (SFArr _ fd1) =
            sfArrG (\a -> let (b,c) = (fdFun fd1) (a,c) in b)
	loopAux sf1 = SF' tf
	    where
		tf dt a = (loopAux sf1', b)
		    where
		        (sf1', (b, c)) = (sfTF' sf1) dt (a, c)

-- | The identity signal function. Use in place of 
--
-- > arr id
identity :: SF a a
identity = SF {sfTF = \a -> (sfId, a)}

-- | The constant signal function. Use 
--
-- > constant x
--
-- in place of
--
-- > arr $ const x
constant :: b -> SF a b
constant b = SF {sfTF = \_ -> (sfConst b, b)}

-- | The time of this part of the signal graph.
-- Note that if a signal function is switched in,
-- the time is relative to the moment of switching,
-- not the moment that animation started.
localTime :: SF a Time
localTime = constant 1.0 >>> integral

-- | identical to 'localTime'
time :: SF a Time
time = localTime

-- | Override the output value for a signal function
-- at the first instant it is processed
(-->) :: b -> SF a b -> SF a b
b0 --> (SF {sfTF = tf10}) = SF {sfTF = \a0 -> (fst (tf10 a0), b0)}

-- | Override the input value for a signal function at the
-- first instant it is processed.
(>--) :: a -> SF a b -> SF a b
a0 >-- (SF {sfTF = tf10}) = SF {sfTF = \_ -> tf10 a0}

-- | Apply a function to the output at the first instant of a signal function
(-=>) :: (b -> b) -> SF a b -> SF a b
f -=> (SF {sfTF = tf10}) =
    SF {sfTF = \a0 -> let (sf1, b0) = tf10 a0 in (sf1, f b0)}

-- | Apply a function to the input at the first instant of a signal function
(>=-) :: (a -> a) -> SF a b -> SF a b
f >=- (SF {sfTF = tf10}) = SF {sfTF = \a0 -> tf10 (f a0)}

-- | Output a value at the first instant, and forever after pass the input
-- value through
initially :: a -- ^ Value at first instant
             -> SF a a
initially = (--> identity)

-- | Signal function:
-- apply a function to an accumulator at each instant. Note that 
-- the output value is the value of the accumulator at each instant.
sscan :: (b -> a -> b ) -- ^ Function from accumulator and input to accumulator
         -> b -- ^ Initial accumulator value
         -> SF a b -- ^ Accumulating scan signal function
sscan f b_init = sscanPrim f' b_init b_init
    where
        f' b a = let b' = f b a in Just (b', b')

-- | Never produce an event
never :: SF a (Event b)
never = SF {sfTF = \_ -> (sfNever, NoEvent)}

-- | Produce an event immediately (at the moment of switching in or animation)
-- and never again.
now :: b -- ^ Value for event
       -> SF a (Event b) -- ^ Signal function producing 
now b0 = (Event b0 --> never)

-- | Produce an event delayed by some time.
after :: Time -- ^ Time to wait before producing event
         -> b -- ^ Value for event
         -> SF a (Event b) -- ^ Signal function producing event after
                           -- specified period
after q x = afterEach [(q,x)]

-- | Produce event every so often (but not immediately)
repeatedly :: Time -- ^ Time between events
              -> b -- ^ Value for all events
              -> SF a (Event b) -- ^ Signal function producing repeated event
repeatedly q x | q > 0 = afterEach qxs
               | otherwise = usrErr "AFRP" "repeatedly" "Non-positive period."
    where
        qxs = (q,x):qxs        

-- | Takes a list of time delays and values to a signal function
-- producing events.
afterEach :: [(Time,b)] -- ^ Time since previous event or start and value for
                        -- event
             -> SF a (Event b)
afterEach qxs = afterEachCat qxs >>> arr (fmap head)

afterEachCat :: [(Time,b)] -> SF a (Event [b])
afterEachCat [] = never
afterEachCat ((q,x):qxs)
    | q < 0     = usrErr "AFRP" "afterEachCat" "Negative period."
    | otherwise = SF {sfTF = tf0}
    where
	tf0 _ = if q <= 0 then
                    emitEventsScheduleNext 0.0 [x] qxs
                else
		    (awaitNextEvent (-q) x qxs, NoEvent)

	emitEventsScheduleNext _ xs [] = (sfNever, Event (reverse xs))
        emitEventsScheduleNext t xs ((q,x):qxs)
	    | q < 0     = usrErr "AFRP" "afterEachCat" "Negative period."
	    | t' >= 0   = emitEventsScheduleNext t' (x:xs) qxs
	    | otherwise = (awaitNextEvent t' x qxs, Event (reverse xs))
	    where
	        t' = t - q
	awaitNextEvent t x qxs = SF' tf
	    where
		tf dt _ | t' >= 0   = emitEventsScheduleNext t' [x] qxs
		        | otherwise = (awaitNextEvent t' x qxs, NoEvent)
		    where
		        t' = t + dt

-- | Delay events passing through                        
delayEvent :: Time -- ^ Time to delay events
              -> SF (Event a) (Event a) -- ^ Signal function delaying events
delayEvent q | q < 0     = usrErr "AFRP" "delayEvent" "Negative delay."
             | q == 0    = identity
             | otherwise = delayEventCat q >>> arr (fmap head)


delayEventCat :: Time -> SF (Event a) (Event [a])
delayEventCat q | q < 0     = usrErr "AFRP" "delayEventCat" "Negative delay."
                | q == 0    = arr (fmap (:[]))
                | otherwise = SF {sfTF = tf0}
    where
        tf0 e = (case e of
                     NoEvent -> noPendingEvent
                     Event x -> pendingEvents (-q) [] [] (-q) x,
                 NoEvent)

        noPendingEvent = SF' tf
            where
                tf _ e = (case e of
                              NoEvent -> noPendingEvent
                              Event x -> pendingEvents (-q) [] [] (-q) x,
                          NoEvent)
				 
        pendingEvents t_last rqxs qxs t_next x = SF' tf
            where
                tf dt e
                    | t_next' >= 0 =
			emitEventsScheduleNext e t_last' rqxs qxs t_next' [x]
                    | otherwise    = 
			(pendingEvents t_last'' rqxs' qxs t_next' x, NoEvent)
                    where
		        t_next' = t_next  + dt
                        t_last' = t_last  + dt 
                        (t_last'', rqxs') =
                            case e of
                                NoEvent  -> (t_last', rqxs)
                                Event x' -> (-q, (t_last'+q,x') : rqxs)

        emitEventsScheduleNext e _ [] [] _ rxs =
            (case e of
                 NoEvent -> noPendingEvent
                 Event x -> pendingEvents (-q) [] [] (-q) x, 
             Event (reverse rxs))
        emitEventsScheduleNext e t_last rqxs [] t_next rxs =
            emitEventsScheduleNext e t_last [] (reverse rqxs) t_next rxs
        emitEventsScheduleNext e t_last rqxs ((q', x') : qxs') t_next rxs
            | q' > t_next = (case e of
                                 NoEvent -> 
				     pendingEvents t_last 
                                                   rqxs 
                                                   qxs'
                                                   (t_next - q')
                                                   x'
                                 Event x'' ->
				     pendingEvents (-q) 
                                                   ((t_last+q, x'') : rqxs)
                                                   qxs'
                                                   (t_next - q')
                                                   x',
                             Event (reverse rxs))
            | otherwise   = emitEventsScheduleNext e
                                                   t_last
                                                   rqxs 
                                                   qxs' 
                                                   (t_next - q')
                                                   (x' : rxs)
-- | Produce an event whenever the input goes from 'False' to 'True'
edge :: SF Bool (Event ())
edge = iEdge True


iEdge :: Bool -> SF Bool (Event ())
iEdge b = sscanPrim f (if b then 2 else 0) NoEvent
    where
        f :: Int -> Bool -> Maybe (Int, Event ())
        f 0 False = Nothing
        f 0 True  = Just (1, Event ())
        f 1 False = Just (0, NoEvent)
        f 1 True  = Just (2, NoEvent)
        f 2 False = Just (0, NoEvent)
        f 2 True  = Nothing
        f _ _     = undefined

-- | Produce an event carrying a specified value whenever
-- the input goes from 'False' to 'True'
edgeTag :: a -- ^ Value for events
           -> SF Bool (Event a)
edgeTag a = edge >>> arr (`tag` a)

-- | Produce the value carried by the Maybe whenever the input goes
-- from 'Nothing' to 'Just'
edgeJust :: SF (Maybe a) (Event a)
edgeJust = edgeBy isJustEdge (Just undefined)
    where
        isJustEdge Nothing  Nothing     = Nothing
        isJustEdge Nothing  ma@(Just _) = ma
        isJustEdge (Just _) (Just _)    = Nothing
        isJustEdge (Just _) Nothing     = Nothing

-- | Compare the input at the current and previous instant 
-- and produce an event based on the comparison
edgeBy :: (a -> a -> Maybe b) -- ^ Comparison function.
                              -- An event will occur at any instant where the 
                              -- value of this function is 'Just'.
          -> a                -- ^ initial \"previous\" instant.
          -> SF a (Event b)   -- ^ Signal function comparing instants
edgeBy isEdge a_init = SF {sfTF = tf0}
    where
	tf0 a0 = (ebAux a0, maybeToEvent (isEdge a_init a0))

	ebAux a_prev = SF' tf
	    where
		tf _ a = (ebAux a, maybeToEvent (isEdge a_prev a))

-- | Suppress a possible event at the instant of animation or switching in
notYet :: SF (Event a) (Event a)
notYet = initially NoEvent

-- | Suppress all but the first event passing through
once :: SF (Event a) (Event a)
once = takeEvents 1

-- | Only permit a certain number of events
takeEvents :: Int -- ^ Number of events to permit
              -> SF (Event a) (Event a) -- ^ Signal function only permitting
                                        -- that many events
takeEvents n | n <= 0 = never
takeEvents n = dSwitch (arr dup) (const (NoEvent >-- takeEvents (n - 1)))

-- | Suppress a certain number of initial events
dropEvents :: Int -- ^ Number of events to suppress initially
              -> SF (Event a) (Event a) -- ^ Signal function suppressing
                                        -- That many events initially
dropEvents n | n <= 0  = identity
dropEvents n = dSwitch (never &&& identity)
                             (const (NoEvent >-- dropEvents (n - 1)))

-- | Switch in a new signal function produced from an event, at the instant
-- of that event.
switch :: SF a (b, Event c) -- ^ Signal function which may eventually produce 
                            -- an event.
          -> (c -> SF a b)  -- ^ Function producing a signal function from the
                            -- event value
          -> SF a b         -- ^ Signal function which may switch to
                            -- a new signal function.
switch (SF {sfTF = tf10}) k = SF {sfTF = tf0}
    where
	tf0 a0 =
	    case tf10 a0 of
	    	(sf1, (b0, NoEvent))  -> (switchAux sf1 k, b0)
		(_,   (_,  Event c0)) -> sfTF (k c0) a0


        switchAux :: SF' a (b, Event c) -> (c -> SF a b) -> SF' a b
	switchAux (SFArr _ (FDC (b, NoEvent))) _ = sfConst b
	switchAux (SFArr _ fd1)                k = switchAuxA1 (fdFun fd1) k
	switchAux sf1                          k = SF' tf
	    where
		tf dt a =
		    case (sfTF' sf1) dt a of
			(sf1', (b, NoEvent)) -> (switchAux sf1' k, b)
			(_,    (_, Event c)) -> sfTF (k c) a

        switchAuxA1 :: (a -> (b, Event c)) -> (c -> SF a b) -> SF' a b
	switchAuxA1 f1 k = sf
	    where
		sf     = SF' tf
		tf _ a =
		    case f1 a of
			(b, NoEvent) -> (sf, b)
			(_, Event c) -> sfTF (k c) a

-- | Decoupled version of 'switch'.
dSwitch :: SF a (b, Event c) -> (c -> SF a b) -> SF a b
dSwitch (SF {sfTF = tf10}) k = SF {sfTF = tf0}
    where
	tf0 a0 =
	    let (sf1, (b0, ec0)) = tf10 a0
            in (case ec0 of
                    NoEvent  -> dSwitchAux sf1 k
		    Event c0 -> fst (sfTF (k c0) a0),
                b0)

        dSwitchAux :: SF' a (b, Event c) -> (c -> SF a b) -> SF' a b
	dSwitchAux (SFArr _ (FDC (b, NoEvent))) _ = sfConst b
	dSwitchAux (SFArr _ fd1)                k = dSwitchAuxA1 (fdFun fd1) k
	dSwitchAux sf1                          k = SF' tf
	    where
		tf dt a =
		    let (sf1', (b, ec)) = (sfTF' sf1) dt a
                    in (case ec of
			    NoEvent -> dSwitchAux sf1' k
			    Event c -> fst (sfTF (k c) a),

			b)

        dSwitchAuxA1 :: (a -> (b, Event c)) -> (c -> SF a b) -> SF' a b
	dSwitchAuxA1 f1 k = sf
	    where
		sf = SF' tf 
		tf _ a =
		    let (b, ec) = f1 a
                    in (case ec of
			    NoEvent -> sf
			    Event c -> fst (sfTF (k c) a),

			b)

-- | Switches in new signal functions carried by input events.
rSwitch :: SF a b                      -- ^ Initial signal function
           -> SF (a, Event (SF a b)) b -- ^ Signal function which may
                                       -- be changed by an event carrying a new
                                       -- signal function
rSwitch sf = switch (first sf) ((noEventSnd >=-) . rSwitch)

-- | Decoupled version of 'rswitch'
drSwitch :: SF a b -> SF (a, Event (SF a b)) b
drSwitch sf = dSwitch (first sf) ((noEventSnd >=-) . drSwitch)

-- This is rather complicated and I'm not sure I understand it.
-- I will document it once I'm sure of how it works. Dr. Nilsson's
-- original comments also expressed skepticism about its correctness
-- and performance. Perhaps it should be removed?
-- | Continuation based switching (undocumented) 
kSwitch :: SF a b -> SF (a,b) (Event c) -> (SF a b -> c -> SF a b) -> SF a b
kSwitch sf10@(SF {sfTF = tf10}) (SF {sfTF = tfe0}) k = SF {sfTF = tf0}
    where
        tf0 a0 =
	    let (sf1, b0) = tf10 a0
            in
	        case tfe0 (a0, b0) of
		    (sfe, NoEvent)  -> (kSwitchAux sf1 sfe, b0)
		    (_,   Event c0) -> sfTF (k sf10 c0) a0

        kSwitchAux (SFArr _ (FDC b)) sfe = kSwitchAuxC1 b sfe
        kSwitchAux (SFArr _ fd1)     sfe = kSwitchAuxA1 (fdFun fd1) sfe
        kSwitchAux sf1 (SFArr _ (FDC NoEvent)) = sf1
        kSwitchAux sf1 (SFArr _ fde) = kSwitchAuxAE sf1 (fdFun fde) 
        kSwitchAux sf1            sfe                 = SF' tf 
	    where
		tf dt a =
		    let	(sf1', b) = (sfTF' sf1) dt a
		    in
		        case (sfTF' sfe) dt (a, b) of
			    (sfe', NoEvent) -> (kSwitchAux sf1' sfe', b)
			    (_,    Event c) -> sfTF (k (freeze sf1 dt) c) a


        kSwitchAuxC1 b (SFArr _ (FDC NoEvent)) = sfConst b
        kSwitchAuxC1 b (SFArr _ fde)        = kSwitchAuxC1AE b (fdFun fde)
        kSwitchAuxC1 b sfe                 = SF' tf 
	    where
		tf dt a =
		    case (sfTF' sfe) dt (a, b) of
			(sfe', NoEvent) -> (kSwitchAuxC1 b sfe', b)
			(_,    Event c) -> sfTF (k (constant b) c) a
        kSwitchAuxA1 f1 (SFArr _ (FDC NoEvent)) = sfArrG f1
        kSwitchAuxA1 f1 (SFArr _ fde)        = kSwitchAuxA1AE f1 (fdFun fde)
        kSwitchAuxA1 f1 sfe                 = SF' tf 
	    where
		tf dt a =
		    let	b = f1 a
		    in
		        case (sfTF' sfe) dt (a, b) of
			    (sfe', NoEvent) -> (kSwitchAuxA1 f1 sfe', b)
			    (_,    Event c) -> sfTF (k (arr f1) c) a

        kSwitchAuxAE (SFArr _ (FDC b))  fe = kSwitchAuxC1AE b fe
        kSwitchAuxAE (SFArr _ fd1)   fe = kSwitchAuxA1AE (fdFun fd1) fe
        kSwitchAuxAE sf1            fe = SF' tf 
	    where
		tf dt a =
		    let	(sf1', b) = (sfTF' sf1) dt a
		    in
		        case fe (a, b) of
			    NoEvent -> (kSwitchAuxAE sf1' fe, b)
			    Event c -> sfTF (k (freeze sf1 dt) c) a

        kSwitchAuxC1AE b fe = SF' tf 
	    where
		tf _ a =
		    case fe (a, b) of
			NoEvent -> (kSwitchAuxC1AE b fe, b)
			Event c -> sfTF (k (constant b) c) a

        kSwitchAuxA1AE f1 fe = SF' tf 
	    where
		tf _ a =
		    let	b = f1 a
		    in
		        case fe (a, b) of
			    NoEvent -> (kSwitchAuxA1AE f1 fe, b)
			    Event c -> sfTF (k (arr f1) c) a

-- | Decoupled version of 'kswitch'
dkSwitch :: SF a b -> SF (a,b) (Event c) -> (SF a b -> c -> SF a b) -> SF a b
dkSwitch sf10@(SF {sfTF = tf10}) (SF {sfTF = tfe0}) k = SF {sfTF = tf0}
    where
        tf0 a0 =
	    let (sf1, b0) = tf10 a0
            in (case tfe0 (a0, b0) of
		    (sfe, NoEvent)  -> dkSwitchAux sf1 sfe
		    (_,   Event c0) -> fst (sfTF (k sf10 c0) a0),
                b0)

        dkSwitchAux sf1 (SFArr _ (FDC NoEvent)) = sf1
        dkSwitchAux sf1 sfe                     = SF' tf 	    
          where
		tf dt a =
		    let	(sf1', b) = (sfTF' sf1) dt a
		    in (case (sfTF' sfe) dt (a, b) of
			    (sfe', NoEvent) -> dkSwitchAux sf1' sfe'
			    (_, Event c) -> fst (sfTF (k (freeze sf1 dt) c) a),
		        b)

broadcast :: Functor col => a -> col sf -> col (a, sf)
broadcast a sfs = fmap (\sf -> (a, sf)) sfs

-- | Broadcast the same output to a collection of signal functions,
-- producing a collection of outputs.
parB :: Functor col => col (SF a b) -> SF a (col b)
parB = par broadcast

pSwitchB :: Functor col =>
    col (SF a b) 
    -> SF (a, col b) (Event c) 
    -> (col (SF a b) -> c -> SF a (col b))
    -> SF a (col b)
pSwitchB = pSwitch broadcast


dpSwitchB :: Functor col =>
    col (SF a b) -> SF (a,col b) (Event c) -> (col (SF a b)->c->SF a (col b))
    -> SF a (col b)
dpSwitchB = dpSwitch broadcast


rpSwitchB :: Functor col =>
    col (SF a b) -> SF (a, Event (col (SF a b) -> col (SF a b))) (col b)
rpSwitchB = rpSwitch broadcast


drpSwitchB :: Functor col =>
    col (SF a b) -> SF (a, Event (col (SF a b) -> col (SF a b))) (col b)
drpSwitchB = drpSwitch broadcast

par :: Functor col =>
    (forall sf . (a -> col sf -> col (b, sf)))
    -> col (SF b c)
    -> SF a (col c)
par rf sfs0 = SF {sfTF = tf0}
    where
	tf0 a0 =
	    let bsfs0 = rf a0 sfs0
		sfcs0 = fmap (\(b0, sf0) -> (sfTF sf0) b0) bsfs0
		sfs   = fmap fst sfcs0
		cs0   = fmap snd sfcs0
	    in
		(parAux rf sfs, cs0)

parAux :: Functor col =>
    (forall sf . (a -> col sf -> col (b, sf)))
    -> col (SF' b c)
    -> SF' a (col c)
parAux rf sfs = SF' tf 
    where
	tf dt a = 
	    let bsfs  = rf a sfs
		sfcs' = fmap (\(b, sf) -> (sfTF' sf) dt b) bsfs
		sfs'  = fmap fst sfcs'
		cs    = fmap snd sfcs'
	    in
	        (parAux rf sfs', cs)

pSwitch :: Functor col =>
    (forall sf . (a -> col sf -> col (b, sf)))
    -> col (SF b c)
    -> SF (a, col c) (Event d)
    -> (col (SF b c) -> d -> SF a (col c))
    -> SF a (col c)
pSwitch rf sfs0 sfe0 k = SF {sfTF = tf0}
    where
	tf0 a0 =
	    let bsfs0 = rf a0 sfs0
		sfcs0 = fmap (\(b0, sf0) -> (sfTF sf0) b0) bsfs0
		sfs   = fmap fst sfcs0
		cs0   = fmap snd sfcs0
	    in
		case (sfTF sfe0) (a0, cs0) of
		    (sfe, NoEvent)  -> (pSwitchAux sfs sfe, cs0)
		    (_,   Event d0) -> sfTF (k sfs0 d0) a0

	pSwitchAux sfs (SFArr _ (FDC NoEvent)) = parAux rf sfs
	pSwitchAux sfs sfe = SF' tf
	    where
		tf dt a =
		    let bsfs  = rf a sfs
			sfcs' = fmap (\(b, sf) -> (sfTF' sf) dt b) bsfs
			sfs'  = fmap fst sfcs'
			cs    = fmap snd sfcs'
		    in
			case (sfTF' sfe) dt (a, cs) of
			    (sfe', NoEvent) -> (pSwitchAux sfs' sfe', cs)
			    (_,    Event d) -> sfTF (k (freezeCol sfs dt) d) a


dpSwitch :: Functor col =>
    (forall sf . (a -> col sf -> col (b, sf)))
    -> col (SF b c)
    -> SF (a, col c) (Event d)
    -> (col (SF b c) -> d -> SF a (col c))
    -> SF a (col c)
dpSwitch rf sfs0 sfe0 k = SF {sfTF = tf0}
    where
	tf0 a0 =
	    let bsfs0 = rf a0 sfs0
		sfcs0 = fmap (\(b0, sf0) -> (sfTF sf0) b0) bsfs0
		cs0   = fmap snd sfcs0
	    in
		(case (sfTF sfe0) (a0, cs0) of
		     (sfe, NoEvent)  -> dpSwitchAux (fmap fst sfcs0) sfe
		     (_,   Event d0) -> fst (sfTF (k sfs0 d0) a0),
	         cs0)

	dpSwitchAux sfs (SFArr _ (FDC NoEvent)) = parAux rf sfs
	dpSwitchAux sfs sfe = SF' tf 
	    where
		tf dt a =
		    let bsfs  = rf a sfs
			sfcs' = fmap (\(b, sf) -> (sfTF' sf) dt b) bsfs
			cs    = fmap snd sfcs'
		    in
			(case (sfTF' sfe) dt (a, cs) of
			     (sfe', NoEvent) -> dpSwitchAux (fmap fst sfcs')
							    sfe'
			     (_,    Event d) -> fst (sfTF (k (freezeCol sfs dt)
							     d)
							  a),
                         cs)


rpSwitch :: Functor col =>
    (forall sf . (a -> col sf -> col (b, sf)))
    -> col (SF b c) -> SF (a, Event (col (SF b c) -> col (SF b c))) (col c)
rpSwitch rf sfs =
    pSwitch (rf . fst) sfs (arr (snd . fst)) $ \sfs' f ->
    noEventSnd >=- rpSwitch rf (f sfs')
drpSwitch :: Functor col =>
    (forall sf . (a -> col sf -> col (b, sf)))
    -> col (SF b c) -> SF (a, Event (col (SF b c) -> col (SF b c))) (col c)
drpSwitch rf sfs =
    dpSwitch (rf . fst) sfs (arr (snd . fst)) $ \sfs' f ->
    noEventSnd >=- drpSwitch rf (f sfs')

-- | For backwards compatability only.
old_hold :: a -> SF (Event a) a
old_hold a_init = switch (constant a_init &&& identity)
                         ((NoEvent >--) . old_hold)

-- | Output the initial value or the value of the last event.
hold :: a -- ^ Initial value
        -> SF (Event a) a -- ^ Signal function which constantly outputs
                       -- the value of the last event.
hold a_init = epPrim f () a_init
    where
        f _ a = ((), a, a)

-- | Decoupled version of 'hold'. Begins outputting event value the instant
-- after the event occurence.
dHold :: a -> SF (Event a) a
dHold a0 = hold a0 >>> iPre a0

-- | Hold the value of a 'Maybe' input.
trackAndHold :: a -- ^ Initial value
                -> SF (Maybe a) a -- ^ Output the initial value or
                                  -- the value of the most recent 'Just'
trackAndHold a_init = arr (maybe NoEvent Event) >>> hold a_init

-- | For backwards compatability only.
old_accum :: a -> SF (Event (a -> a)) (Event a)
old_accum = accumBy (flip ($))

-- | Apply a function carried by an event to an accumulator, producing
-- an event with the new value of the accumulator.
accum :: a -- ^ Initial accumulator value.
         -> SF (Event (a -> a)) (Event a) -- ^ Signal function from events
                                          -- carrying functions to events with
                                          -- the value of those functions 
                                          -- applied to the accumulator
accum a_init = epPrim f a_init NoEvent
    where
        f a g = (a', Event a', NoEvent)
            where
                a' = g a

-- | As with 'accum' but output the value of the accumulator.
accumHold :: a -- ^ Initial value of accumulator
             -> SF (Event (a -> a)) a -- ^ Signal function from events
                                      -- carrying functions to events with
                                      -- the value of those functions applied
                                      -- to the accumulator
accumHold a_init = epPrim f a_init a_init
    where
        f a g = (a', a', a')
            where
                a' = g a

-- | Decoupled version of 'accumHold'. Updated accumulator values begin output 
-- at the instant /after/ the updating event.
dAccumHold :: a -> SF (Event (a -> a)) a
dAccumHold a_init = accumHold a_init >>> iPre a_init

-- | For backwards compatibility only.
old_accumBy :: (b -> a -> b) -> b -> SF (Event a) (Event b)
old_accumBy f b_init = switch (never &&& identity) $ \a -> abAux (f b_init a)
    where
        abAux b = switch (now b &&& notYet) $ \a -> abAux (f b a)

-- | Provide a function and initial accumulator to process events, produce
-- each new accumulator vale as an event.
accumBy :: (b -> a -> b) -- ^ Function from accumulator and event value to
                         -- accumulator.
           -> b          -- ^ Initial accumulator value
           -> SF (Event a) (Event b) -- ^ Signal function processing events
                                     -- with accumulator function
accumBy g b_init = epPrim f b_init NoEvent
    where
        f b a = (b', Event b', NoEvent)
            where
                b' = g b a

-- | As in 'accumBy' but produce the accumulator value as a continuous signal.
accumHoldBy :: (b -> a -> b) -> b -> SF (Event a) b
accumHoldBy g b_init = epPrim f b_init b_init
    where
        f b a = (b', b', b')
            where
                b' = g b a

-- | Decoupled version of 'accumHoldBy'. Output signal changes at the instant
-- /after/ an event.
dAccumHoldBy :: (b -> a -> b) -> b -> SF (Event a) b
dAccumHoldBy f a_init = accumHoldBy f a_init >>> iPre a_init

-- | For backwards compatibility only.
old_accumFilter :: (c -> a -> (c, Maybe b)) -> c -> SF (Event a) (Event b)
old_accumFilter f c_init = switch (never &&& identity) $ \a -> afAux (f c_init a)
    where
        afAux (c, Nothing) = switch (never &&& notYet) $ \a -> afAux (f c a)
        afAux (c, Just b)  = switch (now b &&& notYet) $ \a -> afAux (f c a)

-- | Filter events with an accumulator.
accumFilter :: (c -> a -> (c, Maybe b)) -- ^ Function from accumulator value and
                                        -- event value to new accumulator value
                                        -- and possible event value.
               -> c                     -- ^ Initial accumulator value.
               -> SF (Event a) (Event b) -- ^ Signal function filtering events.
accumFilter g c_init = epPrim f c_init NoEvent
    where
        f c a = case g c a of
                    (c', Nothing) -> (c', NoEvent, NoEvent)
                    (c', Just b)  -> (c', Event b, NoEvent)

-- | For backwards compatibility only.
old_pre :: SF a a
old_pre = SF {sfTF = tf0}
    where
        tf0 a0 = (preAux a0, usrErr "AFRP" "pre" "Uninitialized pre operator.")

	preAux a_prev = SF' tf
	    where
		tf _ a = (preAux a, a_prev)

-- | For backwards compatibility only.
old_iPre :: a -> SF a a
old_iPre = (--> old_pre)

-- | Uninitialized one-instant delay. 
pre :: SF a a
pre = sscanPrim f uninit uninit
    where
        f c a = Just (a, c)
        uninit = usrErr "AFRP" "pre" "Uninitialized pre operator."

-- | Iniitialized one-instant delay
iPre :: a         -- ^ Value of delayed function at first instant
        -> SF a a -- ^ One-instant delay
iPre = (--> pre)

-- | Delay a (non-event) signal by a specific time offsent. For events please
-- use 'delayEvent'.
delay :: Time      -- ^ Time offset to delay signal by
         -> a      -- ^ Initial value until time offset is reached
         -> SF a a -- ^ delayed signal function
delay q a_init | q < 0     = usrErr "AFRP" "delay" "Negative delay."
               | q == 0    = identity
               | otherwise = SF {sfTF = tf0}
    where
        tf0 a0 = (delayAux [] [(q, a0)] 0 a_init, a_init)

        delayAux _ [] _ _ = undefined
        delayAux rbuf buf@((bdt, ba) : buf') t_diff a_prev = SF' tf
            where
                tf dt a | t_diff' < bdt =
                              (delayAux rbuf' buf t_diff' a_prev, a_prev)
                        | otherwise = nextSmpl rbuf' buf' (t_diff' - bdt) ba
                    where
        	        t_diff' = t_diff + dt
        	        rbuf'   = (dt, a) : rbuf
    
                        nextSmpl rbuf [] t_diff a =
                            nextSmpl [] (reverse rbuf) t_diff a
                        nextSmpl rbuf buf@((bdt, ba) : buf') t_diff a
                            | t_diff < bdt = (delayAux rbuf buf t_diff a, a)
                            | otherwise    = nextSmpl rbuf buf' (t_diff-bdt) ba
                
-- | Integrate a signal with respect to time.
{-# INLINE integral #-}
integral :: VectorSpace a s => SF a a
integral = SF {sfTF = tf0}
    where
        igrl0  = zeroVector

	tf0 a0 = (integralAux igrl0 a0, igrl0)

	integralAux igrl a_prev = SF' tf 
	    where
	        tf dt a = (integralAux igrl' a, igrl')
		    where
		       igrl' = igrl ^+^ realToFrac dt *^ a_prev


imIntegral :: VectorSpace a s => a -> SF a a
imIntegral = ((\ _ a' dt v -> v ^+^ realToFrac dt *^ a') `iterFrom`)

iterFrom :: (a -> a -> DTime -> b -> b) -> b -> SF a b
f `iterFrom` b = SF (iterAux b) where
  iterAux b a = (SF' (\ dt a' -> iterAux (f a a' dt b) a'), b)

derivative :: VectorSpace a s => SF a a
derivative = SF {sfTF = tf0}
    where
	tf0 a0 = (derivativeAux a0, zeroVector)

	derivativeAux a_prev = SF' tf
	    where
	        tf dt a = (derivativeAux a, (a ^-^ a_prev) ^/ realToFrac dt)

loopPre :: c -> SF (a,c) (b,c) -> SF a b
loopPre c_init sf = loop (second (iPre c_init) >>> sf)

loopIntegral :: VectorSpace c s => SF (a,c) (b,c) -> SF a b
loopIntegral sf = loop (second integral >>> sf)

noise :: (RandomGen g, Random b) => g -> SF a b
noise g0 = streamToSF (randoms g0)

noiseR :: (RandomGen g, Random b) => (b,b) -> g -> SF a b
noiseR range g0 = streamToSF (randomRs range g0)

streamToSF :: [b] -> SF a b
streamToSF []     = intErr "AFRP" "streamToSF" "Empty list!"
streamToSF (b:bs) = SF {sfTF = tf0}
    where
        tf0 _ = (stsfAux bs, b)

        stsfAux []     = intErr "AFRP" "streamToSF" "Empty list!"
        stsfAux (b:bs) = SF' tf
	    where
		tf _ _ = (stsfAux bs, b)

occasionally :: RandomGen g => g -> Time -> b -> SF a (Event b)
occasionally g t_avg x | t_avg > 0 = SF {sfTF = tf0}
                       | otherwise = usrErr "AFRP" "occasionally"
				            "Non-positive average interval."
    where
    tf0 _ = (occAux ((randoms g) :: [Time]), NoEvent)

    occAux [] = undefined
    occAux (r:rs) = SF' tf
        where
        tf dt _ = let p = 1 - exp (-(dt/t_avg))
                  in (occAux rs, if r < p then Event x else NoEvent)
reactimate :: IO a
	      -> (Bool -> IO (DTime, Maybe a))
	      -> (Bool -> b -> IO Bool)
              -> SF a b
	      -> IO ()
reactimate init sense actuate (SF {sfTF = tf0}) =
    do
        a0 <- init
        let (sf, b0) = tf0 a0
        loop sf a0 b0
    where
        loop sf a b = do
	    done <- actuate True b
            unless (a `seq` b `seq` done) $ do
	        (dt, ma') <- sense False
		let a' = maybe a id ma'
                    (sf', b') = (sfTF' sf) dt a'
		loop sf' a' b'

data ReactState a b = ReactState {
    rsActuate :: ReactHandle a b -> Bool -> b -> IO Bool,
    rsSF :: SF' a b,
    rsA :: a,
    rsB :: b
  }	      

type ReactHandle a b = IORef (ReactState a b)

reactInit :: IO a 
             -> (ReactHandle a b -> Bool -> b -> IO Bool) 
             -> SF a b
             -> IO (ReactHandle a b)
reactInit init actuate (SF {sfTF = tf0}) = 
  do a0 <- init
     let (sf,b0) = tf0 a0
     r <- newIORef (ReactState {rsActuate = actuate, rsSF = sf,
				rsA = a0, rsB = b0 })
     done <- actuate r True b0
     return r

react :: ReactHandle a b
      -> (DTime,Maybe a)
      -> IO Bool
react rh (dt,ma') = 
  do rs@(ReactState {rsActuate = actuate,
	             rsSF = sf,
		     rsA = a,
		     rsB = b }) <- readIORef rh
     let a' = maybe a id ma'
         (sf',b') = (sfTF' sf) dt a'
     writeIORef rh (rs {rsSF = sf',rsA = a',rsB = b'})
     done <- actuate rh True b'
     return done     

embed :: SF a b -> (a, [(DTime, Maybe a)]) -> [b]
embed sf0 (a0, dtas) = b0 : loop a0 sf dtas
    where
	(sf, b0) = (sfTF sf0) a0

        loop _ _ [] = []
	loop a_prev sf ((dt, ma) : dtas) =
	    b : (a `seq` b `seq` (loop a sf' dtas))
	    where
		a        = maybe a_prev id ma
	        (sf', b) = (sfTF' sf) dt a

embedSynch :: SF a b -> (a, [(DTime, Maybe a)]) -> SF Double b
embedSynch sf0 (a0, dtas) = SF {sfTF = tf0}
    where
        tts       = scanl (\t (dt, _) -> t + dt) 0 dtas
	bbs@(b:_) = embed sf0 (a0, dtas)

	tf0 _ = (esAux 0 (zip tts bbs), b)

	esAux _       []    = intErr "AFRP" "embedSynch" "Empty list!"
	esAux tp_prev tbtbs = SF' tf
	    where
		tf dt r | r < 0     = usrErr "AFRP" "embedSynch"
					     "Negative ratio."
			| otherwise = let tp = tp_prev + dt * r
					  (b, tbtbs') = advance tp tbtbs
				      in
					  (esAux tp tbtbs', b)
        advance _  tbtbs@[(_, b)] = (b, tbtbs)
        advance tp tbtbtbs@((_, b) : tbtbs@((t', _) : _))
		    | tp <  t' = (b, tbtbtbs)
		    | t' <= tp = advance tp tbtbs
        advance _ _ = undefined

deltaEncode :: Eq a => DTime -> [a] -> (a, [(DTime, Maybe a)])
deltaEncode _  []        = usrErr "AFRP" "deltaEncode" "Empty input list."
deltaEncode dt aas@(_:_) = deltaEncodeBy (==) dt aas


deltaEncodeBy :: (a -> a -> Bool) -> DTime -> [a] -> (a, [(DTime, Maybe a)])
deltaEncodeBy _  _  []      = usrErr "AFRP" "deltaEncodeBy" "Empty input list."
deltaEncodeBy eq dt (a0:as) = (a0, zip (repeat dt) (debAux a0 as))
    where
	debAux _      []                     = []
	debAux a_prev (a:as) | a `eq` a_prev = Nothing : debAux a as
                             | otherwise     = Just a  : debAux a as 
