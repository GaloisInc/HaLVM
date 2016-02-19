-- |This module defines the generic interface that various HaLVM ports will
-- export to user applications for disks. In some cases, this driver may be
-- hidden behind a file system interface. The standard use case is to use
-- the `DiskDriver` interface provided by your underlying system to find a
-- disk by name. You can then use the normal operations provided by a disk
-- on the returned structure.
--
module HaLVM.Interface.Disk(
         Disk(..)
       , DiskDriver(..)
       )
 where

import Data.ByteString.Lazy as L
import Data.Word

-- |A HaLVM-compatible disk driver.
data DiskDriver = DiskDriver {
       ddriverListDisks      :: IO [String]
     , ddriverInitializeDisk :: String -> IO (Maybe Disk)
     }

instance Monoid DiskDriver where
  mempty = DiskDriver { ddriverListDisks      = return []
                      , ddriverInitializeDisk = const (return Nothing)
                      }
  mappend a b = DiskDriver { ddriverListDisks =
                               do adisks <- ddriverListDisks a
                                  bdisks <- ddriverListDisks b
                                  return (adisks ++ bdisks)
                           , ddriverInitializeDisk =
                               \ x ->
                                  do mdisk <- ddriverInitializeDisk a x
                                     case mdisk of
                                       Nothing -> ddriverInitializeDisk b x
                                       Just _  -> return mdisk
                           }

-- |A HaLVM-compatible disk.
data Disk = Disk {
       diskName                 :: String
     , diskSupportsWrites       :: Bool
     , diskSupportsFlush        :: Bool
     , diskSupportsWriteBarrier :: Bool
     , diskSupportsDiscard      :: Bool
     , diskIsRemovable          :: Bool
     , -- |The number of sectors on the disk
       diskSectors              :: Word64
     , -- |The size of the sectors used on this disk
       diskSectorSize           :: Word
     , -- |Read the given number of sectors from the disk, starting at the
       -- given sector.
       diskRead :: Word64 {- ^ The number of sectors to read    -} ->
                   Word64 {- ^ The sector to start reading from -} ->
                   IO L.ByteString
     , -- |Write the given chunk of data to the given sector, continuing to
       -- write until there is less than a sector of data to write.
       diskWrite :: Word64       {- ^ The sector to start writing to -} ->
                    L.ByteString {- ^The data to write to the disk   -} ->
                    IO ()
     , -- |If "diskSupportsFlush" is True, then this command will flush any
       -- cached writes, and block until the operation completes. Otherwise,
       -- this command has no effect.
       diskFlush :: IO ()
     , -- |If "diskSupportsWriteBarrier" is True, all writes that occur before
       -- this request will be completed before any commands after this event.
       -- If "diskSupportsWriteBarrier" is False, then this command will have
       -- no effect.
       diskWriteBarrier :: IO ()
     , -- |If 'diskSupportsDiscard' is True, then this command indicates that
       -- the given segment(s) are no longer in use, and may be discarded at
       -- any time without impact. It the provided flag is true, this also
       -- ensures that the sector is rendered unrecoverable before the command
       -- returns. If 'diskSupportsDiscard' is False, then this operation has
       -- no effect.
       diskDiscardRegion :: Word64 {- ^ The start sector of the region -} ->
                            Word64 {- ^ The number of sectors in the region -}->
                            Bool   {- ^ Should we render this region
                                        recoverable? -} ->
                            IO ()
     }


