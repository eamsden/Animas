-- |
-- Module      :  FRP.Yampa.Event
-- Copyright   :  (c) Antony Courtney and Henrik Nilsson, Yale University, 2003
-- License     :  BSD-style (see the LICENSE file in the distribution)
--
-- Maintainer  :  nilsson@cs.yale.edu
-- Stability   :  provisional
-- Portability :  portable
--
-- Definition of Yampa Event type and functions on that type.
--

module FRP.Yampa.Event where

import FRP.Yampa.Diagnostics
import FRP.Yampa.Forceable


infixl 8 `tag`, `attach`, `gate`
infixl 7 `joinE`
infixl 6 `lMerge`, `rMerge`, `merge`

-- | Event type
data Event a = NoEvent
	     | Event a

-- | Not an event
noEvent :: Event a
noEvent = NoEvent

-- | Force the first item of a pair to not be an event
noEventFst :: (Event a, b) -- ^ Input pair
              -> (Event c, b) -- ^ No event pair
noEventFst (_, b) = (NoEvent, b)

-- | Force the second item of a pair to not be an event
noEventSnd :: (a, Event b) -- ^ Input pair
              -> (a, Event c) -- ^ No event pair
noEventSnd (a, _) = (a, NoEvent)

instance Eq a => Eq (Event a) where
    NoEvent   == NoEvent   = True
    (Event x) == (Event y) = x == y
    _         == _         = False

instance Ord a => Ord (Event a) where
    compare NoEvent   NoEvent   = EQ
    compare NoEvent   (Event _) = LT
    compare (Event _) NoEvent   = GT
    compare (Event x) (Event y) = compare x y

instance Functor Event where
    fmap _ NoEvent   = NoEvent
    fmap f (Event a) = Event (f a)

instance Forceable a => Forceable (Event a) where
    force ea@NoEvent   = ea
    force ea@(Event a) = force a `seq` ea

-- | Internal: Convert a 'Maybe' value to an event
maybeToEvent :: Maybe a -> Event a
maybeToEvent Nothing  = NoEvent
maybeToEvent (Just a) = Event a

-- | Apply a function to an event, or return a default value
event :: a               -- ^ Default value
         -> (b -> a)     -- ^ Function from event value
         -> Event b      -- ^ Event
         -> a            -- ^ Return value
event a _ NoEvent   = a
event _ f (Event b) = f b

-- | Extract a value from an event. This function will produce an error if
-- applied to a NoEvent function
fromEvent :: Event a -> a
fromEvent (Event a) = a
fromEvent NoEvent   = usrErr "AFRP" "fromEvent" "Not an event."

-- | Predicate: is a value an event occurence
isEvent :: Event a -> Bool
isEvent NoEvent   = False
isEvent (Event _) = True

-- | Predicate: is a value not an event occurence
isNoEvent :: Event a -> Bool
isNoEvent = not . isEvent

-- | Replace a possible event occurence with a new occurence carrying a
-- replacement value
tag :: Event a -- ^ Possible event occurence
       -> b -- ^ Replacement value
       -> Event b
e `tag` b = fmap (const b) e

-- | See above
tagWith :: b -> Event a -> Event b
tagWith = flip tag

-- | Pair a value with an event occurrence's value, creating a new
-- event occurrence
attach :: Event a -> b -> Event (a, b)
e `attach` b = fmap (\a -> (a, b)) e

-- | If both inputs are event occurrences, produce the left event.
lMerge :: Event a -> Event a -> Event a
le `lMerge` re = event re Event le

-- | If both inputs are event occurences, produce the right event.
rMerge :: Event a -> Event a -> Event a
rMerge = flip lMerge

-- | If both inputs are event occurences, produce an error.
merge :: Event a -> Event a -> Event a
merge = mergeBy (usrErr "AFRP" "merge" "Simultaneous event occurrence.")

-- | If both inputs are event occurences, merge them with the supplied
-- function
mergeBy :: (a -> a -> a) -> Event a -> Event a -> Event a
mergeBy _       NoEvent      NoEvent      = NoEvent
mergeBy _       le@(Event _) NoEvent      = le
mergeBy _       NoEvent      re@(Event _) = re
mergeBy resolve (Event l)    (Event r)    = Event (resolve l r)

-- | Apply functions to an event occurences from two sources
mapMerge :: (a -> c) -- ^ Function for occurences in first source
            -> (b -> c) -- ^ Function for occurences in second source
            -> (a -> b -> c) -- ^ Function for occurences in both sources
	    -> Event a -- ^ First source
            -> Event b -- ^ Second source
            -> Event c -- ^ Merged/mapped events
mapMerge _  _  _   NoEvent   NoEvent = NoEvent
mapMerge lf _  _   (Event l) NoEvent = Event (lf l)
mapMerge _  rf _   NoEvent  (Event r) = Event (rf r)
mapMerge _  _  lrf (Event l) (Event r) = Event (lrf l r)

-- | Produce the event occurence closest to the head of the list,
-- if one exists.
mergeEvents :: [Event a] -> Event a
mergeEvents = foldr lMerge NoEvent

-- | From a list of event sources
-- produce an event occurence with a list of values of occurrences
catEvents :: [Event a] -> Event [a]
catEvents eas = case [ a | Event a <- eas ] of
		    [] -> NoEvent
		    as -> Event as

-- | If there is an occurence from both sources, produce an occurence
-- with both values.
joinE :: Event a -> Event b -> Event (a,b)
joinE NoEvent   _         = NoEvent
joinE _         NoEvent   = NoEvent
joinE (Event l) (Event r) = Event (l,r)

-- | Create a pair of event occurences from a single event occurence
-- with a pair of values
splitE :: Event (a,b) -> (Event a, Event b)
splitE NoEvent       = (NoEvent, NoEvent)
splitE (Event (a,b)) = (Event a, Event b)

-- | Apply a predicate to event occurences and forward them only if
-- it matches
filterE :: (a -> Bool) -> Event a -> Event a
filterE p e@(Event a) = if (p a) then e else NoEvent
filterE _ NoEvent     = NoEvent

-- | Apply a 'Maybe' function to event occurences,
-- producing events only for 'Just' values.
mapFilterE :: (a -> Maybe b) -> Event a -> Event b
mapFilterE _ NoEvent   = NoEvent
mapFilterE f (Event a) = case f a of
			    Nothing -> NoEvent
			    Just b  -> Event b

-- | Only pass through events if some external condition is true.
gate :: Event a -> Bool -> Event a
_ `gate` False = NoEvent
e `gate` True  = e
