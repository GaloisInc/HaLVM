-- Copyright 2013 Galois, Inc.
-- This software is distributed under a standard, three-clause BSD license.
-- Please see the file LICENSE, distributed with this software, for specific
-- terms and conditions.
-- |Module lifting a low-level CPUMap to Haskell
module Hypervisor.Structures.CPUMap(
         CPUMap
       , emptyCPUMap
       , clearCPUMap
       , fillCPUMap
       , addCPU
       , removeCPU
       , elemCPUMap
       , maxPossibleCPUNumInMap
       , unionCPUMaps
       , intersectCPUMaps
       , withCPUMapWriter
       , readSerializedCPUMap
       , sizeOfSerializedCPUMap
       )
 where

import Control.Exception
import Data.Bits
import Data.List
import Data.Maybe
import Data.Word
import Foreign.Marshal.Alloc
import Foreign.Ptr
import Foreign.Storable

#define __XEN__
#include <stdint.h>
#include <xen/xen.h>

data CPUMap = CPUMap Int Integer

instance Eq CPUMap where
  (CPUMap _ a) == (CPUMap _ b) = a == b
  (CPUMap _ a) /= (CPUMap _ b) = a /= b

instance Show CPUMap where
  show (CPUMap nr a) =
    "<" ++ intercalate "," (catMaybes (map mShowCPU [0..nr-1])) ++ ">"
   where mShowCPU n | testBit a n = Just ("cpu" ++ show n)
                    | otherwise   = Nothing

-- |Create an empty CPU map with the maximum CPU of (n - 1).
emptyCPUMap :: Int -> CPUMap
emptyCPUMap nr = CPUMap nr 0

-- |Remove all CPUs from the map
clearCPUMap :: CPUMap -> CPUMap
clearCPUMap (CPUMap nr _) = CPUMap nr 0

-- |Turn on all CPUs in the map
fillCPUMap :: CPUMap -> CPUMap
fillCPUMap (CPUMap nr bits) = CPUMap nr (foldl setBit bits [0..nr-1])

-- |Add the given CPU number to the map
addCPU :: CPUMap -> Int -> CPUMap
addCPU (CPUMap nr bits) n
  | n >= nr   = error "Invalid CPU to add to CPU Map"
  | otherwise = CPUMap nr (setBit bits n)

-- |Remove the given CPU number from the map
removeCPU :: CPUMap -> Int -> CPUMap
removeCPU (CPUMap nr bits) n
  | n >= nr   = error "Invalid CPU to remove from CPU Map"
  | otherwise = CPUMap nr (clearBit bits n)

elemCPUMap :: CPUMap -> Int -> Bool
elemCPUMap (CPUMap nr bits) n
  | n >= nr   = error "Invalid CPU to test in CPU Map"
  | otherwise = testBit bits n

-- |Return the maximum CPU number the map can contain
maxPossibleCPUNumInMap :: CPUMap -> Int
maxPossibleCPUNumInMap (CPUMap n _) = n - 1

-- |Union together two maps
unionCPUMaps :: CPUMap -> CPUMap -> CPUMap
unionCPUMaps (CPUMap n1 b1) (CPUMap n2 b2) = CPUMap (max n1 n2) (b1 .|. b2)

-- |Intersect two CPU maps.
intersectCPUMaps :: CPUMap -> CPUMap -> CPUMap
intersectCPUMaps (CPUMap n1 b1) (CPUMap n2 b2) = CPUMap (max n1 n2) (b1 .&. b2)

-- |Synthesize a CPUMap serialization routine for use within the given
-- function. Saving the routine and using it later will cause big problems,
-- so don't do it.
withCPUMapWriter :: CPUMap -> ((Ptr a -> IO ()) -> IO b) -> IO b
withCPUMapWriter (CPUMap num bits) thunk =
  bracket (mallocBytes ((num + 7) `div` 8)) free $ \ mapPtr -> do
    writeBytes ((num + 7) `div` 8) (castPtr mapPtr) bits
    thunk (writer mapPtr)
 where
  writer mapPtr ptr = do
    (#poke struct xenctl_cpumap,bitmap)  ptr mapPtr
    (#poke struct xenctl_cpumap,nr_cpus) ptr (fromIntegral num :: Word32)
  writeBytes 0 _   _   = return ()
  writeBytes x p val = do
    poke p (fromIntegral (val .&. 0xFF) :: Word8)
    writeBytes (x - 1) (p `plusPtr` 1) (val `shiftR` 8)

-- |Read a serialized CPUMap into our internal format. After this function
-- returns, the pointer (and its sub pointers) are no longer referenced.
readSerializedCPUMap :: Ptr CPUMap -> IO CPUMap
readSerializedCPUMap ptr = do
  nr_cpus <- (#peek struct xenctl_cpumap,nr_cpus) ptr
  valptr  <- (#peek struct xenctl_cpumap,bitmap) ptr
  intval  <- readBytes ((nr_cpus + 7) `div` 8) valptr
  return (CPUMap (fromIntegral nr_cpus) intval)
 where
  readBytes :: Word32 -> Ptr Word8 -> IO Integer
  readBytes 0 _ = return 0
  readBytes x valptr = do
    cur  <- peek valptr
    rest <- readBytes (x - 1) (valptr `plusPtr` 1)
    return (fromIntegral cur .|. (rest `shiftL` 8))

-- |The size of a serialized CPU map in memory, not counting the internal
-- data structures.
sizeOfSerializedCPUMap :: Int
sizeOfSerializedCPUMap  = (#size struct xenctl_cpumap)

