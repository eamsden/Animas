-- |
-- Module      :  FRP.Yampa.Diagnostics
-- Copyright   :  (c) Antony Courtney and Henrik Nilsson, Yale University, 2003
-- License     :  BSD-style (see the LICENSE file in the distribution)
--
-- Maintainer  :  nilsson@cs.yale.edu
-- Stability   :  provisional
-- Portability :  portable
--
-- Standardized error-reporting for Yampa

module FRP.Yampa.Diagnostics where

-- | Error created by improper usage
usrErr :: String -- ^ Module name
          -> String -- ^ Function name
          -> String -- ^ Error message
          -> a 
usrErr mn fn msg = error (mn ++ "." ++ fn ++ ": " ++ msg)

-- | Error internal to yampa (a bug)
intErr :: String -- ^ Module name
          -> String -- ^ Function name
          -> String -- ^ Error message
          -> a
intErr mn fn msg = error ("[internal error] " ++ mn ++ "." ++ fn ++ ": "
                          ++ msg)
