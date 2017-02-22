{-
    Copyright 2017 Joshua Rombauer

    This file is part of Monky.

    Monky is free software: you can redistribute it and/or modify
    it under the terms of the GNU Lesser General Public License as published by
    the Free Software Foundation, either version 3 of the License, or
    (at your option) any later version.

    Monky is distributed in the hope that it will be useful,
    but WITHOUT ANY WARRANTY; without even the implied warranty of
    MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
    GNU Lesser General Public License for more details.

    You should have received a copy of the GNU Lesser General Public License
    along with Monky.  If not, see <http://www.gnu.org/licenses/>.
-}
{-# LANGUAGE OverloadedStrings #-}
{-|
Module      : Monky.Outputs/Lemonbar
Description : Output module for lemonbar
Maintainer  : ongy
Stability   : testing
Portability : Linux

This module provides the output generation for piping into a lemonbar
-}
module Monky.Outputs.Lemonbar
  ( LemonOutput
  , getLemonOut
  , getLemonOut'
  )
where

import System.Directory(getCurrentDirectory)
import System.IO (hFlush, stdout)
import Monky.Modules
import Monky.Outputs.Unicode

import Data.Text (Text)
import qualified Data.Text as T
import qualified Data.Text.IO as T
import Data.Monoid ((<>))
import Control.Monad (unless)

-- |The output handle for lemonbar pipe
data LemonOutput = LemonOutput MonkyOut

doOut :: MonkyOut -> IO ()
doOut (MonkyPlain t) = T.putStr t
-- No real Image or bar support. lemonbar does understand utf8 though.
doOut (MonkyImage _ char) = putChar char
doOut (MonkyBar p) = putChar $ barChar p
doOut (MonkyHBar p) = do
  putStr $ replicate (p `div` 10) '█'
  putChar $ hBarChar (p `mod` 10 * 10)
doOut (MonkyColor (f, b) o) = do
  unless (T.null f) $ T.putStr $ "%{F" <> f <> "}"
  unless (T.null b) $ T.putStr $ "%{B" <> b <> "}"
  doOut o
  -- Only reset the colors which were changed
  unless (T.null f) $ T.putStr "%{F-}"
  unless (T.null b) $ T.putStr "%{B-}"

doSegment :: [MonkyOut] -> IO ()
doSegment = mapM_ doOut

instance MonkyOutput LemonOutput where
  doLine _ [] = error "Why are you calling doLine without any modules? I don't think your config makes sense"
  doLine _ [x] = do
    doSegment x
    putStr "\n"
    hFlush stdout
  doLine l@(LemonOutput d) (x:xs) = do
    doSegment x
    doOut d
    doLine l xs

-- |Get an output handle for lemonbar formatting
-- Assumes @" | "@ as divider
getLemonOut' :: IO LemonOutput
getLemonOut' = getLemonOut $ MonkyPlain " | "


-- |Get an output handle for lemonbar formatting
getLemonOut
  :: MonkyOut -- ^Divider
  -> IO LemonOutput
getLemonOut = return . LemonOutput
