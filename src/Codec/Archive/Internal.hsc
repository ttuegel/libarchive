{-# LANGUAGE ForeignFunctionInterface #-}

module Codec.Archive.Internal where

import Data.Int
import Foreign.C.String
import Foreign.C.Types
import Foreign.Ptr ( Ptr )
import System.Posix.Types

#include <archive.h>
#include <archive_entry.h>


data Archive

data ArchiveEntry

type ArchiveFormat = CInt

data LinkResolver


archiveEOF :: CInt
archiveEOF = #{const ARCHIVE_EOF}

archiveOK :: CInt
archiveOK = #{const ARCHIVE_OK}


foreign import ccall "archive.h archive_free"
  archive_free :: Ptr Archive -> IO CInt


foreign import ccall "archive.h archive_read_new"
  archive_read_new :: IO (Ptr Archive)

foreign import ccall "archive.h archive_read_support_filter_all"
  archive_read_support_filter_all :: Ptr Archive -> IO CInt

foreign import ccall "archive.h archive_read_open_fd"
  archive_read_open_fd :: Ptr Archive -> Fd -> CSize -> IO CInt

foreign import ccall "archive.h archive_read_data_block"
  archive_read_data_block :: Ptr Archive
                          -> Ptr () -> Ptr CSize -> Ptr Int64 -> IO CInt

foreign import ccall "archive.h archive_read_next_header2"
  archive_read_next_header2 :: Ptr Archive -> Ptr ArchiveEntry -> IO CInt


foreign import ccall "archive.h archive_write_new"
  archive_write_new :: IO (Ptr Archive)

foreign import ccall "archive.h archive_write_open_fd"
  archive_write_open_fd :: Ptr Archive -> Fd -> IO CInt

foreign import ccall "archive.h archive_write_add_filter_b64encode"
  archive_write_add_filter_b64encode :: Ptr Archive -> IO CInt

foreign import ccall "archive.h archive_write_add_filter_bzip2"
  archive_write_add_filter_bzip2 :: Ptr Archive -> IO CInt

foreign import ccall "archive.h archive_write_add_filter_compress"
  archive_write_add_filter_compress :: Ptr Archive -> IO CInt

foreign import ccall "archive.h archive_write_add_filter_grzip"
  archive_write_add_filter_grzip :: Ptr Archive -> IO CInt

foreign import ccall "archive.h archive_write_add_filter_gzip"
  archive_write_add_filter_gzip :: Ptr Archive -> IO CInt

foreign import ccall "archive.h archive_write_add_filter_lrzip"
  archive_write_add_filter_lrzip :: Ptr Archive -> IO CInt

foreign import ccall "archive.h archive_write_add_filter_lz4"
  archive_write_add_filter_lz4 :: Ptr Archive -> IO CInt

foreign import ccall "archive.h archive_write_add_filter_lzip"
  archive_write_add_filter_lzip :: Ptr Archive -> IO CInt

foreign import ccall "archive.h archive_write_add_filter_lzma"
  archive_write_add_filter_lzma :: Ptr Archive -> IO CInt

foreign import ccall "archive.h archive_write_add_filter_lzop"
  archive_write_add_filter_lzop :: Ptr Archive -> IO CInt

foreign import ccall "archive.h archive_write_add_filter_none"
  archive_write_add_filter_none :: Ptr Archive -> IO CInt

foreign import ccall "archive.h archive_write_add_filter_uuencode"
  archive_write_add_filter_uuencode :: Ptr Archive -> IO CInt

foreign import ccall "archive.h archive_write_add_filter_xz"
  archive_write_add_filter_xz :: Ptr Archive -> IO CInt

foreign import ccall "archive.h archive_write_set_format_7zip"
  archive_write_set_format_7zip :: Ptr Archive -> IO CInt

foreign import ccall "archive.h archive_write_set_format_ar_bsd"
  archive_write_set_format_ar_bsd :: Ptr Archive -> IO CInt

foreign import ccall "archive.h archive_write_set_format_ar_svr4"
  archive_write_set_format_ar_svr4 :: Ptr Archive -> IO CInt

foreign import ccall "archive.h archive_write_set_format_cpio"
  archive_write_set_format_cpio :: Ptr Archive -> IO CInt

foreign import ccall "archive.h archive_write_set_format_cpio_newc"
  archive_write_set_format_cpio_newc :: Ptr Archive -> IO CInt

foreign import ccall "archive.h archive_write_set_format_gnutar"
  archive_write_set_format_gnutar :: Ptr Archive -> IO CInt

foreign import ccall "archive.h archive_write_set_format_iso9660"
  archive_write_set_format_iso9660 :: Ptr Archive -> IO CInt

foreign import ccall "archive.h archive_write_set_format_mtree"
  archive_write_set_format_mtree :: Ptr Archive -> IO CInt

foreign import ccall "archive.h archive_write_set_format_mtree_classic"
  archive_write_set_format_mtree_classic :: Ptr Archive -> IO CInt

foreign import ccall "archive.h archive_write_set_format_pax"
  archive_write_set_format_pax :: Ptr Archive -> IO CInt

foreign import ccall "archive.h archive_write_set_format_pax_restricted"
  archive_write_set_format_pax_restricted :: Ptr Archive -> IO CInt

foreign import ccall "archive.h archive_write_set_format_raw"
  archive_write_set_format_raw :: Ptr Archive -> IO CInt

foreign import ccall "archive.h archive_write_set_format_shar"
  archive_write_set_format_shar :: Ptr Archive -> IO CInt

foreign import ccall "archive.h archive_write_set_format_shar_dump"
  archive_write_set_format_shar_dump :: Ptr Archive -> IO CInt

foreign import ccall "archive.h archive_write_set_format_ustar"
  archive_write_set_format_ustar :: Ptr Archive -> IO CInt

foreign import ccall "archive.h archive_write_set_format_v7tar"
  archive_write_set_format_v7tar :: Ptr Archive -> IO CInt

foreign import ccall "archive.h archive_write_set_format_warc"
  archive_write_set_format_warc :: Ptr Archive -> IO CInt

foreign import ccall "archive.h archive_write_set_format_xar"
  archive_write_set_format_xar :: Ptr Archive -> IO CInt

foreign import ccall "archive.h archive_write_set_format_zip"
  archive_write_set_format_zip :: Ptr Archive -> IO CInt

foreign import ccall "archive.h archive_write_header"
  archive_write_header :: Ptr Archive -> Ptr ArchiveEntry -> IO CInt

foreign import ccall "archive.h archive_write_data"
  archive_write_data :: Ptr Archive -> Ptr () -> CSize -> IO CSize

foreign import ccall "archive.h archive_write_finish_entry"
  archive_write_finish_entry :: Ptr Archive -> IO CInt


foreign import ccall "archive.h archive_format"
  archive_format :: Ptr Archive -> IO ArchiveFormat

foreign import ccall "archive_entry.h archive_entry_clear"
  archive_entry_clear :: Ptr ArchiveEntry -> IO (Ptr ArchiveEntry)

foreign import ccall "archive_entry.h archive_entry_clone"
  archive_entry_clone :: Ptr ArchiveEntry -> IO (Ptr ArchiveEntry)

foreign import ccall "archive_entry.h archive_entry_new"
  archive_entry_new :: IO (Ptr ArchiveEntry)

foreign import ccall "archive_entry.h archive_entry_free"
  archive_entry_free :: Ptr ArchiveEntry -> IO ()


foreign import ccall "archive_entry.h archive_entry_acl_clear"
  archive_entry_acl_clear :: Ptr ArchiveEntry -> IO ()

foreign import ccall "archive_entry.h archive_entry_acl_add_entry"
  archive_entry_acl_add_entry :: Ptr ArchiveEntry
                              -> CInt -> CInt -> CInt -> CInt
                              -> CString -> IO CInt

foreign import ccall "archive_entry.h archive_entry_acl_add_entry_w"
  archive_entry_acl_add_entry_w :: Ptr ArchiveEntry
                                -> CInt -> CInt -> CInt -> CInt
                                -> CWString -> IO CInt

foreign import ccall "archive_entry.h archive_entry_acl_reset"
  archive_entry_acl_reset :: Ptr ArchiveEntry -> IO CInt

foreign import ccall "archive_entry.h archive_entry_acl_next"
  archive_entry_acl_next :: Ptr ArchiveEntry -> CInt
                         -> Ptr CInt -> Ptr CInt -> Ptr CInt -> Ptr CInt
                         -> Ptr CString -> IO ()

foreign import ccall "archive_entry.h archive_entry_acl_next_w"
  archive_entry_acl_next_w :: Ptr ArchiveEntry -> CInt
                           -> Ptr CInt -> Ptr CInt -> Ptr CInt -> Ptr CInt
                           -> Ptr CWString -> IO ()

foreign import ccall "archive_entry.h archive_entry_acl_count"
  archive_entry_acl_count :: Ptr ArchiveEntry -> CInt -> IO CInt


foreign import ccall "archive_entry.h archive_entry_xattr_clear"
  archive_entry_xattr_clear :: Ptr ArchiveEntry -> IO ()

foreign import ccall "archive_entry.h archive_entry_xattr_add_entry"
  archive_entry_xattr_add_entry :: Ptr ArchiveEntry
                                -> CString -> CString -> CSize -> IO ()

foreign import ccall "archive_entry.h archive_entry_xattr_count"
  archive_entry_xattr_count :: Ptr ArchiveEntry -> IO CInt

foreign import ccall "archive_entry.h archive_entry_xattr_reset"
  archive_entry_xattr_reset :: Ptr ArchiveEntry -> IO CInt

foreign import ccall "archive_entry.h archive_entry_xattr_next"
  archive_entry_xattr_next :: Ptr ArchiveEntry
                           -> Ptr CString -> Ptr () -> Ptr CSize
                           -> IO CInt


foreign import ccall "archive_entry.h archive_entry_sparse_clear"
  archive_entry_sparse_clear :: Ptr ArchiveEntry -> IO ()

foreign import ccall "archive_entry.h archive_entry_sparse_add_entry"
  archive_entry_sparse_add_entry :: Ptr ArchiveEntry
                                 -> Int64 -> Int64 -> IO ()

foreign import ccall "archive_entry.h archive_entry_sparse_count"
  archive_entry_sparse_count :: Ptr ArchiveEntry -> IO CInt

foreign import ccall "archive_entry.h archive_entry_sparse_reset"
  archive_entry_sparse_reset :: Ptr ArchiveEntry -> IO CInt

foreign import ccall "archive_entry.h archive_entry_sparse_next"
  archive_entry_sparse_next :: Ptr ArchiveEntry
                            -> Ptr Int64 -> Ptr Int64 -> IO CInt


foreign import ccall "archive_entry.h archive_entry_linkresolver_new"
  archive_entry_linkresolver_new :: IO (Ptr LinkResolver)

foreign import ccall "archive_entry.h archive_entry_linkresolver_free"
  archive_entry_linkresolver_free :: Ptr LinkResolver -> IO ()

foreign import ccall "archive_entry.h archive_entry_linkresolver_set_strategy"
  archive_entry_linkresolver_set_strategy :: Ptr LinkResolver
                                          -> ArchiveFormat
                                          -> IO ()


foreign import ccall "archive_entry.h archive_entry_hardlink"
  archive_entry_hardlink :: Ptr ArchiveEntry -> IO CString

foreign import ccall "archive_entry.h archive_entry_hardlink_w"
  archive_entry_hardlink_w :: Ptr ArchiveEntry -> IO CWString

foreign import ccall "archive_entry.h archive_entry_copy_hardlink"
  archive_entry_copy_hardlink :: Ptr ArchiveEntry -> CString -> IO ()

foreign import ccall "archive_entry.h archive_entry_copy_hardlink_w"
  archive_entry_copy_hardlink_w :: Ptr ArchiveEntry -> CWString -> IO ()


foreign import ccall "archive_entry.h archive_entry_pathname"
  archive_entry_pathname :: Ptr ArchiveEntry -> IO CString

foreign import ccall "archive_entry.h archive_entry_pathname_w"
  archive_entry_pathname_w :: Ptr ArchiveEntry -> IO CWString

foreign import ccall "archive_entry.h archive_entry_copy_pathname"
  archive_entry_copy_pathname :: Ptr ArchiveEntry -> CString -> IO ()

foreign import ccall "archive_entry.h archive_entry_copy_pathname_w"
  archive_entry_copy_pathname_w :: Ptr ArchiveEntry -> CWString -> IO ()


foreign import ccall "archive_entry.h archive_entry_sourcepath"
  archive_entry_sourcepath :: Ptr ArchiveEntry -> IO CString

foreign import ccall "archive_entry.h archive_entry_sourcepath_w"
  archive_entry_sourcepath_w :: Ptr ArchiveEntry -> IO CWString

foreign import ccall "archive_entry.h archive_entry_copy_sourcepath"
  archive_entry_copy_sourcepath :: Ptr ArchiveEntry -> CString -> IO ()

foreign import ccall "archive_entry.h archive_entry_copy_sourcepath_w"
  archive_entry_copy_sourcepath_w :: Ptr ArchiveEntry -> CWString -> IO ()


foreign import ccall "archive_entry.h archive_entry_symlink"
  archive_entry_symlink :: Ptr ArchiveEntry -> IO CString

foreign import ccall "archive_entry.h archive_entry_symlink_w"
  archive_entry_symlink_w :: Ptr ArchiveEntry -> IO CWString

foreign import ccall "archive_entry.h archive_entry_copy_symlink"
  archive_entry_copy_symlink :: Ptr ArchiveEntry -> CString -> IO ()

foreign import ccall "archive_entry.h archive_entry_copy_symlink_w"
  archive_entry_copy_symlink_w :: Ptr ArchiveEntry -> CWString -> IO ()


foreign import ccall "archive_entry.h archive_entry_gid"
  archive_entry_gid :: Ptr ArchiveEntry -> IO CGid

foreign import ccall "archive_entry.h archive_entry_set_gid"
  archive_entry_set_gid :: Ptr ArchiveEntry -> CGid -> IO ()


foreign import ccall "archive_entry.h archive_entry_uid"
  archive_entry_uid :: Ptr ArchiveEntry -> IO CUid

foreign import ccall "archive_entry.h archive_entry_set_uid"
  archive_entry_set_uid :: Ptr ArchiveEntry -> CUid -> IO ()


foreign import ccall "archive_entry.h archive_entry_filetype"
  archive_entry_filetype :: Ptr ArchiveEntry -> IO CMode

foreign import ccall "archive_entry.h archive_entry_set_filetype"
  archive_entry_set_filetype :: Ptr ArchiveEntry -> CMode -> IO ()


foreign import ccall "archive_entry.h archive_entry_mode"
  archive_entry_mode :: Ptr ArchiveEntry -> IO CMode

foreign import ccall "archive_entry.h archive_entry_set_mode"
  archive_entry_set_mode :: Ptr ArchiveEntry -> CMode -> IO ()


foreign import ccall "archive_entry.h archive_entry_size"
  archive_entry_size :: Ptr ArchiveEntry -> IO Int64

foreign import ccall "archive_entry.h archive_entry_set_size"
  archive_entry_set_size :: Ptr ArchiveEntry -> Int64 -> IO ()

foreign import ccall "archive_entry.h archive_entry_unset_size"
  archive_entry_unset_size :: Ptr ArchiveEntry -> IO ()

foreign import ccall "archive_entry.h archive_entry_size_is_set"
  archive_entry_size_is_set :: Ptr ArchiveEntry -> IO CInt


foreign import ccall "archive_entry.h archive_entry_dev"
  archive_entry_dev :: Ptr ArchiveEntry -> IO CDev

foreign import ccall "archive_entry.h archive_entry_set_dev"
  archive_entry_set_dev :: Ptr ArchiveEntry -> CDev -> IO ()

foreign import ccall "archive_entry.h archive_entry_dev_is_set"
  archive_entry_dev_is_set :: Ptr ArchiveEntry -> IO CInt


foreign import ccall "archive_entry.h archive_entry_ino64"
  archive_entry_ino64 :: Ptr ArchiveEntry -> IO Int64

foreign import ccall "archive_entry.h archive_entry_set_ino64"
  archive_entry_set_ino64 :: Ptr ArchiveEntry -> Int64 -> IO ()


foreign import ccall "archive_entry.h archive_entry_nlink"
  archive_entry_nlink :: Ptr ArchiveEntry -> IO CNlink

foreign import ccall "archive_entry.h archive_entry_set_nlink"
  archive_entry_set_nlink :: Ptr ArchiveEntry -> CNlink -> IO ()


foreign import ccall "archive_entry.h archive_entry_rdev"
  archive_entry_rdev :: Ptr ArchiveEntry -> IO CDev

foreign import ccall "archive_entry.h archive_entry_set_rdev"
  archive_entry_set_rdev :: Ptr ArchiveEntry -> CDev -> IO ()


foreign import ccall "archive_entry.h archive_entry_atime"
  archive_entry_atime :: Ptr ArchiveEntry -> IO CTime

foreign import ccall "archive_entry.h archive_entry_atime_nsec"
  archive_entry_atime_nsec :: Ptr ArchiveEntry -> IO CLong

foreign import ccall "archive_entry.h archive_entry_set_atime"
  archive_entry_set_atime :: Ptr ArchiveEntry -> CTime -> CLong -> IO ()


foreign import ccall "archive_entry.h archive_entry_birthtime"
  archive_entry_birthtime :: Ptr ArchiveEntry -> IO CTime

foreign import ccall "archive_entry.h archive_entry_birthtime_nsec"
  archive_entry_birthtime_nsec :: Ptr ArchiveEntry -> IO CLong

foreign import ccall "archive_entry.h archive_entry_set_birthtime"
  archive_entry_set_birthtime :: Ptr ArchiveEntry -> CTime -> CLong -> IO ()


foreign import ccall "archive_entry.h archive_entry_ctime"
  archive_entry_ctime :: Ptr ArchiveEntry -> IO CTime

foreign import ccall "archive_entry.h archive_entry_ctime_nsec"
  archive_entry_ctime_nsec :: Ptr ArchiveEntry -> IO CLong

foreign import ccall "archive_entry.h archive_entry_set_ctime"
  archive_entry_set_ctime :: Ptr ArchiveEntry -> CTime -> CLong -> IO ()


foreign import ccall "archive_entry.h archive_entry_mtime"
  archive_entry_mtime :: Ptr ArchiveEntry -> IO CTime

foreign import ccall "archive_entry.h archive_entry_mtime_nsec"
  archive_entry_mtime_nsec :: Ptr ArchiveEntry -> IO CLong

foreign import ccall "archive_entry.h archive_entry_set_mtime"
  archive_entry_set_mtime :: Ptr ArchiveEntry -> CTime -> CLong -> IO ()
