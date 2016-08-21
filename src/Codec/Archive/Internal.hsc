{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE ForeignFunctionInterface #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE StandaloneDeriving #-}

module Codec.Archive.Internal where

import Control.Exception
import Data.Int
import Data.Typeable
import Foreign.C.Error
import Foreign.C.String
import Foreign.C.Types
import Foreign.Ptr ( Ptr )
import System.Posix.Types

#include <archive.h>
#include <archive_entry.h>


data Archive

data LinkResolver


archiveEOF :: CInt
archiveEOF = #{const ARCHIVE_EOF}

archiveOK :: CInt
archiveOK = #{const ARCHIVE_OK}


deriving instance Show Errno

data ArchiveError = ArchiveError Errno String
  deriving ( Show, Typeable )

instance Exception ArchiveError


checkArchiveError :: Ptr Archive -> CInt -> IO Bool
checkArchiveError p err
  | err < archiveOK = do
      errno <- Errno <$> archive_errno p
      errstr <- archive_error_string p >>= peekCAString
      throwIO (ArchiveError errno errstr)
  | err == archiveOK = pure False
  | otherwise = pure True


foreign import ccall "archive.h archive_errno"
  archive_errno :: Ptr Archive -> IO CInt

foreign import ccall "archive.h archive_error_string"
  archive_error_string :: Ptr Archive -> IO CString


checkArchiveError_ :: Ptr Archive -> CInt -> IO ()
checkArchiveError_ p err = checkArchiveError p err >> pure ()


foreign import ccall "archive.h archive_free"
  archive_free :: Ptr Archive -> IO CInt


foreign import ccall "archive.h archive_read_new"
  archive_read_new :: IO (Ptr Archive)

foreign import ccall "archive.h archive_read_support_filter_all"
  archive_read_support_filter_all :: Ptr Archive -> IO CInt

foreign import ccall "archive.h archive_read_open_fd"
  archive_read_open_fd :: Ptr Archive -> Fd -> CSize -> IO CInt

foreign import ccall "archive.h archive_read_data"
  archive_read_data :: Ptr Archive -> Ptr () -> CSize -> IO CInt

foreign import ccall "archive.h archive_read_next_header2"
  archive_read_next_header2 :: Ptr Archive -> Ptr Entry -> IO CInt


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
  archive_write_header :: Ptr Archive -> Ptr Entry -> IO CInt

foreign import ccall "archive.h archive_write_data"
  archive_write_data :: Ptr Archive -> Ptr () -> CSize -> IO CSize

foreign import ccall "archive.h archive_write_finish_entry"
  archive_write_finish_entry :: Ptr Archive -> IO CInt


foreign import ccall "archive.h archive_format"
  archive_format :: Ptr Archive -> IO CInt

foreign import ccall "archive_entry.h archive_entry_clear"
  archive_entry_clear :: Ptr Entry -> IO (Ptr Entry)

foreign import ccall "archive_entry.h archive_entry_clone"
  archive_entry_clone :: Ptr Entry -> IO (Ptr Entry)

foreign import ccall "archive_entry.h archive_entry_new"
  archive_entry_new :: IO (Ptr Entry)

foreign import ccall "archive_entry.h archive_entry_free"
  archive_entry_free :: Ptr Entry -> IO ()


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


data TimeSpec = TimeSpec { sec :: !EpochTime
                         , nsec :: !Int64
                         }


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


setWriteFormat :: Ptr Archive -> Format -> IO ()
setWriteFormat p fmt = set >>= checkArchiveError_ p where
  set =
    case fmt of
      Format7zip -> archive_write_set_format_7zip p
      FormatArBSD -> archive_write_set_format_ar_bsd p
      FormatArSvr4 -> archive_write_set_format_ar_svr4 p
      FormatCpio -> archive_write_set_format_cpio p
      FormatCpioNewc -> archive_write_set_format_cpio_newc p
      FormatGNUtar -> archive_write_set_format_gnutar p
      FormatISO9660 -> archive_write_set_format_iso9660 p
      FormatMtree -> archive_write_set_format_mtree p
      FormatMtreeClassic -> archive_write_set_format_mtree_classic p
      FormatPax -> archive_write_set_format_pax p
      FormatPaxRestricted -> archive_write_set_format_pax_restricted p
      FormatRaw -> archive_write_set_format_raw p
      FormatShar -> archive_write_set_format_shar p
      FormatSharDump -> archive_write_set_format_shar_dump p
      FormatUStar -> archive_write_set_format_ustar p
      FormatV7tar -> archive_write_set_format_v7tar p
      FormatWARC -> archive_write_set_format_warc p
      FormatXar -> archive_write_set_format_xar p
      FormatZip -> archive_write_set_format_zip p


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


addWriteFilter :: Ptr Archive -> Filter -> IO ()
addWriteFilter p flt = add >>= checkArchiveError_ p where
  add =
    case flt of
      FilterB64encode -> archive_write_add_filter_b64encode p
      FilterBzip2 -> archive_write_add_filter_bzip2 p
      FilterCompress -> archive_write_add_filter_compress p
      FilterGrzip -> archive_write_add_filter_grzip p
      FilterGzip -> archive_write_add_filter_gzip p
      FilterLrzip -> archive_write_add_filter_lrzip p
      FilterLz4 -> archive_write_add_filter_lz4 p
      FilterLzip -> archive_write_add_filter_lzip p
      FilterLzop -> archive_write_add_filter_lzop p
      FilterNone -> archive_write_add_filter_none p
      FilterUuencode -> archive_write_add_filter_uuencode p
      FilterXz -> archive_write_add_filter_xz p


setWriteFilters :: Ptr Archive -> [Filter] -> IO ()
setWriteFilters p [] = addWriteFilter p FilterNone
setWriteFilters p fs = mapM_ (addWriteFilter p) fs
