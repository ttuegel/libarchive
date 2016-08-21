{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE ForeignFunctionInterface #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE StandaloneDeriving #-}

module Codec.Archive.Internal.Error
       ( checkArchiveError, checkArchiveError_
       , checkArchiveReadError, checkArchiveWriteError
       ) where

import Control.Exception
import Foreign.C.Error
import Foreign.C.String
import Foreign.C.Types
import Foreign.Ptr ( Ptr )

import Codec.Archive.Internal.Types


#include <archive.h>
#include <archive_entry.h>

foreign import ccall "archive.h archive_errno"
  archive_errno :: Ptr (Archive rw) -> IO CInt

foreign import ccall "archive.h archive_error_string"
  archive_error_string :: Ptr (Archive rw) -> IO CString

archiveOK :: CInt
archiveOK = #{const ARCHIVE_OK}


checkArchiveError :: Ptr (Archive rw) -> CInt -> IO Bool
checkArchiveError p err
  | err < archiveOK = archiveError p
  | err == archiveOK = pure False
  | otherwise = pure True


archiveError :: Ptr (Archive rw) -> IO a
archiveError p = do
  errno <- Errno <$> archive_errno p
  errstr <- archive_error_string p >>= peekCAString
  throwIO (ArchiveError errno errstr)


checkArchiveError_ :: Ptr (Archive rw) -> CInt -> IO ()
checkArchiveError_ p err | err < 0 = archiveError p
                         | otherwise = pure ()


checkArchiveWriteError :: Ptr (Archive rw) -> CSize -> IO ()
checkArchiveWriteError p err | err < 0 = archiveError p
                             | otherwise = pure ()


checkArchiveReadError :: Ptr (Archive rw) -> CSize -> IO Bool
checkArchiveReadError p err | err < 0 = archiveError p
                            | otherwise = pure (err == 0)
