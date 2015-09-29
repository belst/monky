{-
    Copyright 2015 Markus Ongyerth, Stephan Guenther

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
{-|
Module      : Monky.Network
Description : Allows access to information about they systems network
Maintainer  : ongy
Stability   : testing
Portability : Linux

This module allows to add multiple network interfaces.
If multiple network interfaces are active the first one in the list will be
used.
-}
module Monky.Network (NetworkHandles, getReadWriteMulti, getNetworkHandles)
where


import Data.Time.Clock.POSIX
import Monky.Utility
import Data.IORef

-- |Internal handle represanting exactly one interface
data NetworkHandle = NetH File File File (IORef Int) (IORef Int) (IORef POSIXTime)
-- |The handle exported by this module
data NetworkHandles = NetHs [NetworkHandle]

basePath :: String
basePath = "/sys/class/net/"

readPath :: String
readPath = "/statistics/rx_bytes"

writePath :: String
writePath = "/statistics/tx_bytes"

statePath :: String
statePath = "/operstate"

getReadWriteReal :: NetworkHandle -> IO (Int, Int)
getReadWriteReal (NetH readf writef _ readref writeref timeref) = do
  nread <- readValue readf
  nwrite <- readValue writef
  time <- getPOSIXTime
  oread <- readIORef readref
  owrite <- readIORef writeref
  otime <- readIORef timeref
  let cread = oread - nread
  let cwrite = owrite - nwrite
  let ctime = otime - time
  writeIORef readref nread
  writeIORef writeref nwrite
  writeIORef timeref time
  return ((cread * 8) `sdiv` round ctime,
    (cwrite * 8) `sdiv` round ctime)
  where sdiv x 0 = x
        sdiv x y = x `div` y

getReadWrite :: NetworkHandle -> IO (Maybe (Int, Int))
getReadWrite (NetH readf writef statef readref writeref timeref) = do
  state <- readLine statef
  if state == "down"
    then return Nothing
    else do
      val <- getReadWriteReal (NetH readf writef statef readref writeref timeref)
      return $Just val

getMultiReadWriteInt :: [NetworkHandle] -> IO (Maybe (Int, Int))
getMultiReadWriteInt [] = return Nothing
getMultiReadWriteInt (x:xs) = do
  val <- getReadWrite x
  case val of
    Nothing -> getMultiReadWriteInt xs
    _ -> return val

{- |Get the read/write rate of the first interface in the list that is connected

The value will be a tuple (Read, Write) in bit/s or 'Nothing' if no network is
connected.
-}
getReadWriteMulti :: NetworkHandles -> IO (Maybe (Int, Int))
getReadWriteMulti (NetHs xs) = getMultiReadWriteInt xs

getNetworkHandle :: String -> IO NetworkHandle
getNetworkHandle dev = do
  readf <- fopen $path ++ readPath
  writef <- fopen $path ++ writePath
  statef <- fopen $path ++ statePath
  readref <- newIORef (1 :: Int)
  writeref <- newIORef (1 :: Int)
  timeref <- newIORef (0 :: POSIXTime)
  return $NetH readf writef statef readref writeref timeref
  where path = basePath ++ dev

-- |Create a 'NetworkHandles' for the list of networks
getNetworkHandles :: [String]  -- ^A list of interface names, the display order will be the same as the order in this list
                  -> IO NetworkHandles
getNetworkHandles [] = return $NetHs []
getNetworkHandles (x:xs) = do
  handle <- getNetworkHandle x
  (NetHs ns) <- getNetworkHandles xs
  return (NetHs (handle:ns))
