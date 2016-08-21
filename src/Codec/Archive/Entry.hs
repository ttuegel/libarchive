{-# LANGUAGE ForeignFunctionInterface #-}
{-# LANGUAGE RecordWildCards #-}

module Codec.Archive.Entry
       ( withEntry, peekEntry, pokeEntry ) where

import Control.Exception
import Data.Int
import Foreign.C.String
import Foreign.C.Types
import Foreign.Ptr ( Ptr )
import System.Posix.Types

import Codec.Archive.Types


foreign import ccall "archive_entry.h archive_entry_new"
  archive_entry_new :: IO (Ptr Entry)

foreign import ccall "archive_entry.h archive_entry_free"
  archive_entry_free :: Ptr Entry -> IO ()

withEntry :: (Ptr Entry -> IO a) -> IO a
withEntry = bracket archive_entry_new archive_entry_free


foreign import ccall "archive_entry.h archive_entry_acl_clear"
  archive_entry_acl_clear :: Ptr Entry -> IO ()

foreign import ccall "archive_entry.h archive_entry_acl_add_entry"
  archive_entry_acl_add_entry :: Ptr Entry
                              -> CInt -> CInt -> CInt -> CInt
                              -> CString -> IO CInt

foreign import ccall "archive_entry.h archive_entry_acl_add_entry_w"
  archive_entry_acl_add_entry_w :: Ptr Entry
                                -> CInt -> CInt -> CInt -> CInt
                                -> CWString -> IO CInt

foreign import ccall "archive_entry.h archive_entry_acl_reset"
  archive_entry_acl_reset :: Ptr Entry -> IO CInt

foreign import ccall "archive_entry.h archive_entry_acl_next"
  archive_entry_acl_next :: Ptr Entry -> CInt
                         -> Ptr CInt -> Ptr CInt -> Ptr CInt -> Ptr CInt
                         -> Ptr CString -> IO ()

foreign import ccall "archive_entry.h archive_entry_acl_next_w"
  archive_entry_acl_next_w :: Ptr Entry -> CInt
                           -> Ptr CInt -> Ptr CInt -> Ptr CInt -> Ptr CInt
                           -> Ptr CWString -> IO ()

foreign import ccall "archive_entry.h archive_entry_acl_count"
  archive_entry_acl_count :: Ptr Entry -> CInt -> IO CInt


foreign import ccall "archive_entry.h archive_entry_xattr_clear"
  archive_entry_xattr_clear :: Ptr Entry -> IO ()

foreign import ccall "archive_entry.h archive_entry_xattr_add_entry"
  archive_entry_xattr_add_entry :: Ptr Entry
                                -> CString -> CString -> CSize -> IO ()

foreign import ccall "archive_entry.h archive_entry_xattr_count"
  archive_entry_xattr_count :: Ptr Entry -> IO CInt

foreign import ccall "archive_entry.h archive_entry_xattr_reset"
  archive_entry_xattr_reset :: Ptr Entry -> IO CInt

foreign import ccall "archive_entry.h archive_entry_xattr_next"
  archive_entry_xattr_next :: Ptr Entry
                           -> Ptr CString -> Ptr () -> Ptr CSize
                           -> IO CInt


foreign import ccall "archive_entry.h archive_entry_sparse_clear"
  archive_entry_sparse_clear :: Ptr Entry -> IO ()

foreign import ccall "archive_entry.h archive_entry_sparse_add_entry"
  archive_entry_sparse_add_entry :: Ptr Entry
                                 -> Int64 -> Int64 -> IO ()

foreign import ccall "archive_entry.h archive_entry_sparse_count"
  archive_entry_sparse_count :: Ptr Entry -> IO CInt

foreign import ccall "archive_entry.h archive_entry_sparse_reset"
  archive_entry_sparse_reset :: Ptr Entry -> IO CInt

foreign import ccall "archive_entry.h archive_entry_sparse_next"
  archive_entry_sparse_next :: Ptr Entry
                            -> Ptr Int64 -> Ptr Int64 -> IO CInt


foreign import ccall "archive_entry.h archive_entry_linkresolver_new"
  archive_entry_linkresolver_new :: IO (Ptr LinkResolver)

foreign import ccall "archive_entry.h archive_entry_linkresolver_free"
  archive_entry_linkresolver_free :: Ptr LinkResolver -> IO ()

foreign import ccall "archive_entry.h archive_entry_linkresolver_set_strategy"
  archive_entry_linkresolver_set_strategy :: Ptr LinkResolver -> CInt -> IO ()


foreign import ccall "archive_entry.h archive_entry_hardlink_w"
  archive_entry_hardlink_w :: Ptr Entry -> IO CWString

foreign import ccall "archive_entry.h archive_entry_copy_hardlink_w"
  archive_entry_copy_hardlink_w :: Ptr Entry -> CWString -> IO ()


foreign import ccall "archive_entry.h archive_entry_pathname_w"
  archive_entry_pathname_w :: Ptr Entry -> IO CWString

foreign import ccall "archive_entry.h archive_entry_copy_pathname_w"
  archive_entry_copy_pathname_w :: Ptr Entry -> CWString -> IO ()


foreign import ccall "archive_entry.h archive_entry_sourcepath_w"
  archive_entry_sourcepath_w :: Ptr Entry -> IO CWString

foreign import ccall "archive_entry.h archive_entry_copy_sourcepath_w"
  archive_entry_copy_sourcepath_w :: Ptr Entry -> CWString -> IO ()


foreign import ccall "archive_entry.h archive_entry_symlink_w"
  archive_entry_symlink_w :: Ptr Entry -> IO CWString

foreign import ccall "archive_entry.h archive_entry_copy_symlink_w"
  archive_entry_copy_symlink_w :: Ptr Entry -> CWString -> IO ()


foreign import ccall "archive_entry.h archive_entry_gid"
  archive_entry_gid :: Ptr Entry -> IO CGid

foreign import ccall "archive_entry.h archive_entry_set_gid"
  archive_entry_set_gid :: Ptr Entry -> CGid -> IO ()


foreign import ccall "archive_entry.h archive_entry_uid"
  archive_entry_uid :: Ptr Entry -> IO CUid

foreign import ccall "archive_entry.h archive_entry_set_uid"
  archive_entry_set_uid :: Ptr Entry -> CUid -> IO ()


foreign import ccall "archive_entry.h archive_entry_filetype"
  archive_entry_filetype :: Ptr Entry -> IO CMode

foreign import ccall "archive_entry.h archive_entry_set_filetype"
  archive_entry_set_filetype :: Ptr Entry -> CMode -> IO ()


foreign import ccall "archive_entry.h archive_entry_mode"
  archive_entry_mode :: Ptr Entry -> IO CMode

foreign import ccall "archive_entry.h archive_entry_set_mode"
  archive_entry_set_mode :: Ptr Entry -> CMode -> IO ()


foreign import ccall "archive_entry.h archive_entry_size"
  archive_entry_size :: Ptr Entry -> IO Int64

foreign import ccall "archive_entry.h archive_entry_set_size"
  archive_entry_set_size :: Ptr Entry -> Int64 -> IO ()


foreign import ccall "archive_entry.h archive_entry_dev"
  archive_entry_dev :: Ptr Entry -> IO CDev

foreign import ccall "archive_entry.h archive_entry_set_dev"
  archive_entry_set_dev :: Ptr Entry -> CDev -> IO ()


foreign import ccall "archive_entry.h archive_entry_ino64"
  archive_entry_ino64 :: Ptr Entry -> IO Int64

foreign import ccall "archive_entry.h archive_entry_set_ino64"
  archive_entry_set_ino64 :: Ptr Entry -> Int64 -> IO ()


foreign import ccall "archive_entry.h archive_entry_nlink"
  archive_entry_nlink :: Ptr Entry -> IO CNlink

foreign import ccall "archive_entry.h archive_entry_set_nlink"
  archive_entry_set_nlink :: Ptr Entry -> CNlink -> IO ()


foreign import ccall "archive_entry.h archive_entry_rdev"
  archive_entry_rdev :: Ptr Entry -> IO CDev

foreign import ccall "archive_entry.h archive_entry_set_rdev"
  archive_entry_set_rdev :: Ptr Entry -> CDev -> IO ()


foreign import ccall "archive_entry.h archive_entry_atime"
  archive_entry_atime :: Ptr Entry -> IO CTime

foreign import ccall "archive_entry.h archive_entry_atime_nsec"
  archive_entry_atime_nsec :: Ptr Entry -> IO CLong

foreign import ccall "archive_entry.h archive_entry_set_atime"
  archive_entry_set_atime :: Ptr Entry -> CTime -> CLong -> IO ()


foreign import ccall "archive_entry.h archive_entry_birthtime"
  archive_entry_birthtime :: Ptr Entry -> IO CTime

foreign import ccall "archive_entry.h archive_entry_birthtime_nsec"
  archive_entry_birthtime_nsec :: Ptr Entry -> IO CLong

foreign import ccall "archive_entry.h archive_entry_set_birthtime"
  archive_entry_set_birthtime :: Ptr Entry -> CTime -> CLong -> IO ()


foreign import ccall "archive_entry.h archive_entry_ctime"
  archive_entry_ctime :: Ptr Entry -> IO CTime

foreign import ccall "archive_entry.h archive_entry_ctime_nsec"
  archive_entry_ctime_nsec :: Ptr Entry -> IO CLong

foreign import ccall "archive_entry.h archive_entry_set_ctime"
  archive_entry_set_ctime :: Ptr Entry -> CTime -> CLong -> IO ()


foreign import ccall "archive_entry.h archive_entry_mtime"
  archive_entry_mtime :: Ptr Entry -> IO CTime

foreign import ccall "archive_entry.h archive_entry_mtime_nsec"
  archive_entry_mtime_nsec :: Ptr Entry -> IO CLong

foreign import ccall "archive_entry.h archive_entry_set_mtime"
  archive_entry_set_mtime :: Ptr Entry -> CTime -> CLong -> IO ()

peekEntry :: Ptr Entry -> IO Entry
peekEntry p = do
  hardlink <- archive_entry_hardlink_w p >>= peekCWString
  pathname <- archive_entry_pathname_w p >>= peekCWString
  sourcepath <- archive_entry_sourcepath_w p >>= peekCWString
  symlink <- archive_entry_symlink_w p >>= peekCWString
  mode <- archive_entry_mode p
  filetype <- archive_entry_filetype p
  gid <- archive_entry_gid p
  uid <- archive_entry_uid p
  size <- archive_entry_size p
  dev <- archive_entry_dev p
  ino64 <- archive_entry_ino64 p
  nlink <- archive_entry_nlink p
  rdev <- archive_entry_rdev p
  atime <- do
    sec <- archive_entry_atime p
    nsec <- fromIntegral <$> archive_entry_atime_nsec p
    pure TimeSpec {..}
  mtime <- do
    sec <- archive_entry_mtime p
    nsec <- fromIntegral <$> archive_entry_mtime_nsec p
    pure TimeSpec {..}
  ctime <- do
    sec <- archive_entry_ctime p
    nsec <- fromIntegral <$> archive_entry_ctime_nsec p
    pure TimeSpec {..}
  birthtime <- do
    sec <- archive_entry_birthtime p
    nsec <- fromIntegral <$> archive_entry_birthtime_nsec p
    pure TimeSpec {..}
  pure Entry {..}

pokeEntry :: Ptr Entry -> Entry -> IO ()
pokeEntry p (Entry {..}) = do
  withCWString hardlink $ archive_entry_copy_hardlink_w p
  withCWString pathname $ archive_entry_copy_pathname_w p
  withCWString sourcepath $ archive_entry_copy_sourcepath_w p
  withCWString symlink $ archive_entry_copy_symlink_w p
  archive_entry_set_mode p mode
  archive_entry_set_filetype p filetype
  archive_entry_set_gid p gid
  archive_entry_set_uid p uid
  archive_entry_set_size p size
  archive_entry_set_dev p dev
  archive_entry_set_ino64 p ino64
  archive_entry_set_nlink p nlink
  archive_entry_set_rdev p rdev
  case atime of
    TimeSpec {..} -> archive_entry_set_atime p sec (fromIntegral nsec)
  case ctime of
    TimeSpec {..} -> archive_entry_set_ctime p sec (fromIntegral nsec)
  case mtime of
    TimeSpec {..} -> archive_entry_set_mtime p sec (fromIntegral nsec)
  case birthtime of
    TimeSpec {..} -> archive_entry_set_birthtime p sec (fromIntegral nsec)
