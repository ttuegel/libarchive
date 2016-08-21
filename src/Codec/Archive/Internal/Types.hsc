{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}

module Codec.Archive.Internal.Types
       ( ArchiveError(..)
       , Archive, R, W
       , Entry(..), ACL(..), ACLType(..), aclTypeMask, ACLTag(..)
       , XAttr(..)
       , TimeSpec(..)
       , Format(..), Filter(..)
         -- * Courtesy exports
       , DeviceID
       , EpochTime
       , Errno(..)
       , FileMode
       , GroupID
       , LinkCount
       , Int32, Int64
       , UserID
       ) where

import Control.Exception ( Exception )
import Data.Bits
import Data.ByteString ( ByteString )
import Data.Int ( Int32, Int64 )
import Data.Typeable
import Foreign.C.Error ( Errno(..) )
import Foreign.C.Types ( CInt )
import System.Posix.Types
       ( DeviceID, EpochTime, FileMode, GroupID, LinkCount, UserID )

#include <archive_entry.h>


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
                   , acls :: [ACL]
                   , xattrs :: [XAttr]
                   }

data ACL = ACL { aclType :: !ACLType
               , permset :: !Int32
               , tag :: !ACLTag
               , qualifier :: !Int32
               , name :: !String
               }

data ACLType = TypeAccess
             | TypeDefault
             | TypeAllow
             | TypeDeny
             | TypeAudit
             | TypeAlarm

instance Enum ACLType where
  fromEnum TypeAccess = #{const ARCHIVE_ENTRY_ACL_TYPE_ACCESS}
  fromEnum TypeDefault = #{const ARCHIVE_ENTRY_ACL_TYPE_DEFAULT}
  fromEnum TypeAllow = #{const ARCHIVE_ENTRY_ACL_TYPE_ALLOW}
  fromEnum TypeDeny = #{const ARCHIVE_ENTRY_ACL_TYPE_DENY}
  fromEnum TypeAudit = #{const ARCHIVE_ENTRY_ACL_TYPE_AUDIT}
  fromEnum TypeAlarm = #{const ARCHIVE_ENTRY_ACL_TYPE_ALARM}

  toEnum #{const ARCHIVE_ENTRY_ACL_TYPE_ACCESS} = TypeAccess
  toEnum #{const ARCHIVE_ENTRY_ACL_TYPE_DEFAULT} = TypeDefault
  toEnum #{const ARCHIVE_ENTRY_ACL_TYPE_ALLOW} = TypeAllow
  toEnum #{const ARCHIVE_ENTRY_ACL_TYPE_DENY} = TypeDeny
  toEnum #{const ARCHIVE_ENTRY_ACL_TYPE_AUDIT} = TypeAudit
  toEnum #{const ARCHIVE_ENTRY_ACL_TYPE_ALARM} = TypeAlarm
  toEnum t = error ("unknown ACL type " ++ show t)

aclTypeMask :: CInt
aclTypeMask = #{const ARCHIVE_ENTRY_ACL_TYPE_POSIX1E}
              .|. #{const ARCHIVE_ENTRY_ACL_TYPE_NFS4}

data ACLTag = TagUser
            | TagUserObj
            | TagGroup
            | TagGroupObj
            | TagMask
            | TagOther
            | TagEveryone

instance Enum ACLTag where
  fromEnum TagUser = #{const ARCHIVE_ENTRY_ACL_USER}
  fromEnum TagUserObj = #{const ARCHIVE_ENTRY_ACL_USER_OBJ}
  fromEnum TagGroup = #{const ARCHIVE_ENTRY_ACL_GROUP}
  fromEnum TagGroupObj = #{const ARCHIVE_ENTRY_ACL_GROUP_OBJ}
  fromEnum TagMask = #{const ARCHIVE_ENTRY_ACL_MASK}
  fromEnum TagOther = #{const ARCHIVE_ENTRY_ACL_OTHER}
  fromEnum TagEveryone = #{const ARCHIVE_ENTRY_ACL_EVERYONE}

  toEnum #{const ARCHIVE_ENTRY_ACL_USER} = TagUser
  toEnum #{const ARCHIVE_ENTRY_ACL_USER_OBJ} = TagUserObj
  toEnum #{const ARCHIVE_ENTRY_ACL_GROUP} = TagGroup
  toEnum #{const ARCHIVE_ENTRY_ACL_GROUP_OBJ} = TagGroupObj
  toEnum #{const ARCHIVE_ENTRY_ACL_MASK} = TagMask
  toEnum #{const ARCHIVE_ENTRY_ACL_OTHER} = TagOther
  toEnum #{const ARCHIVE_ENTRY_ACL_EVERYONE} = TagEveryone
  toEnum t = error ("unknown ACL tag " ++ show t)

data XAttr = XAttr { name :: !String
                   , value :: !ByteString
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
