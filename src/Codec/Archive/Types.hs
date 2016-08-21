{-# LANGUAGE StandaloneDeriving #-}

module Codec.Archive.Types
       ( ArchiveError(..)
       , Archive, R, W
       , LinkResolver
       , Entry(..), TimeSpec(..)
       , Format(..), Filter(..)
         -- * Courtesy exports
       , DeviceID
       , EpochTime
       , Errno(..)
       , FileMode
       , GroupID
       , LinkCount
       , Int64
       , UserID
       ) where

import Control.Exception ( Exception )
import Data.Int ( Int64 )
import Data.Typeable
import Foreign.C.Error ( Errno(..) )
import System.Posix.Types
       ( DeviceID, EpochTime, FileMode, GroupID, LinkCount, UserID )


deriving instance Show Errno


-- | 'ArchiveError' is thrown if there is an error processing the archive. The
-- exception provides the error code and an informational message.
data ArchiveError = ArchiveError Errno String
  deriving ( Show, Typeable )

instance Exception ArchiveError


data Archive rw  -- ^ A handle to an open archive. The phantom type parameter
                 -- @rw@ is one of 'R' or 'W' and indicates whether the archive
                 -- is open for reading or writing.

data R  -- ^ 'R' indicates an 'Archive' open for reading.
data W  -- ^ 'W' indicates an 'Archive' open for writing.


data LinkResolver


-- | An 'Entry' represents one file header in an archive.
data Entry = Entry { hardlink :: !FilePath
                   , pathname :: !FilePath
                   , sourcepath :: !FilePath
                   , symlink :: !FilePath
                   , filetype :: !FileMode
                   , mode :: !FileMode
                   , gid :: !GroupID
                   , uid :: !UserID
                   , size :: !Int64
                   , dev :: !DeviceID
                   , ino64 :: !Int64
                   , nlink :: !LinkCount
                   , rdev :: !DeviceID
                   , atime :: !TimeSpec
                   , mtime :: !TimeSpec
                   , ctime :: !TimeSpec
                   , birthtime :: !TimeSpec
                   }

-- | 'TimeSpec' specifies a time with nanosecond resolution.
data TimeSpec = TimeSpec { sec :: !EpochTime
                         , nsec :: !Int64
                         }


data Format = Format7zip
            | FormatArBSD
            | FormatArSvr4
            | FormatCpio
            | FormatCpioNewc
            | FormatGNUtar
            | FormatISO9660
            | FormatMtree
            | FormatMtreeClassic
            | FormatPax
            | FormatPaxRestricted
            | FormatRaw
            | FormatShar
            | FormatSharDump
            | FormatUStar
            | FormatV7tar
            | FormatWARC
            | FormatXar
            | FormatZip


data Filter = FilterB64encode
            | FilterBzip2
            | FilterCompress
            | FilterGrzip
            | FilterGzip
            | FilterLrzip
            | FilterLz4
            | FilterLzip
            | FilterLzma
            | FilterLzop
            | FilterNone
            | FilterUuencode
            | FilterXz
