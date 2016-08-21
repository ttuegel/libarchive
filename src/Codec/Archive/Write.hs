{-# LANGUAGE ForeignFunctionInterface #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE ViewPatterns #-}

module Codec.Archive.Write
       ( withArchiveWrite
       , archiveWriteEntry, archiveWriteData
       , module Codec.Archive.Entry
       , module Codec.Archive.Error
       , module Codec.Archive.Types
       ) where

import Control.Exception
import Data.ByteString ( ByteString )
import qualified Data.ByteString as B
import Data.Int
import Data.Typeable
import Foreign.C.Error
import Foreign.C.String
import Foreign.C.Types
import Foreign.Ptr ( Ptr, castPtr )
import System.Posix.Types

import Codec.Archive.Error
import Codec.Archive.Entry ( pokeEntry, withEntry )
import Codec.Archive.Types


foreign import ccall "archive.h archive_free"
  archive_free :: Ptr (Archive rw) -> IO CInt

foreign import ccall "archive.h archive_write_new"
  archive_write_new :: IO (Ptr (Archive W))

foreign import ccall "archive.h archive_write_open_fd"
  archive_write_open_fd :: Ptr (Archive W) -> Fd -> IO CInt

foreign import ccall "archive.h archive_write_add_filter_b64encode"
  archive_write_add_filter_b64encode :: Ptr (Archive W) -> IO CInt

foreign import ccall "archive.h archive_write_add_filter_bzip2"
  archive_write_add_filter_bzip2 :: Ptr (Archive W) -> IO CInt

foreign import ccall "archive.h archive_write_add_filter_compress"
  archive_write_add_filter_compress :: Ptr (Archive W) -> IO CInt

foreign import ccall "archive.h archive_write_add_filter_grzip"
  archive_write_add_filter_grzip :: Ptr (Archive W) -> IO CInt

foreign import ccall "archive.h archive_write_add_filter_gzip"
  archive_write_add_filter_gzip :: Ptr (Archive W) -> IO CInt

foreign import ccall "archive.h archive_write_add_filter_lrzip"
  archive_write_add_filter_lrzip :: Ptr (Archive W) -> IO CInt

foreign import ccall "archive.h archive_write_add_filter_lz4"
  archive_write_add_filter_lz4 :: Ptr (Archive W) -> IO CInt

foreign import ccall "archive.h archive_write_add_filter_lzip"
  archive_write_add_filter_lzip :: Ptr (Archive W) -> IO CInt

foreign import ccall "archive.h archive_write_add_filter_lzma"
  archive_write_add_filter_lzma :: Ptr (Archive W) -> IO CInt

foreign import ccall "archive.h archive_write_add_filter_lzop"
  archive_write_add_filter_lzop :: Ptr (Archive W) -> IO CInt

foreign import ccall "archive.h archive_write_add_filter_none"
  archive_write_add_filter_none :: Ptr (Archive W) -> IO CInt

foreign import ccall "archive.h archive_write_add_filter_uuencode"
  archive_write_add_filter_uuencode :: Ptr (Archive W) -> IO CInt

foreign import ccall "archive.h archive_write_add_filter_xz"
  archive_write_add_filter_xz :: Ptr (Archive W) -> IO CInt

foreign import ccall "archive.h archive_write_set_format_7zip"
  archive_write_set_format_7zip :: Ptr (Archive W) -> IO CInt

foreign import ccall "archive.h archive_write_set_format_ar_bsd"
  archive_write_set_format_ar_bsd :: Ptr (Archive W) -> IO CInt

foreign import ccall "archive.h archive_write_set_format_ar_svr4"
  archive_write_set_format_ar_svr4 :: Ptr (Archive W) -> IO CInt

foreign import ccall "archive.h archive_write_set_format_cpio"
  archive_write_set_format_cpio :: Ptr (Archive W) -> IO CInt

foreign import ccall "archive.h archive_write_set_format_cpio_newc"
  archive_write_set_format_cpio_newc :: Ptr (Archive W) -> IO CInt

foreign import ccall "archive.h archive_write_set_format_gnutar"
  archive_write_set_format_gnutar :: Ptr (Archive W) -> IO CInt

foreign import ccall "archive.h archive_write_set_format_iso9660"
  archive_write_set_format_iso9660 :: Ptr (Archive W) -> IO CInt

foreign import ccall "archive.h archive_write_set_format_mtree"
  archive_write_set_format_mtree :: Ptr (Archive W) -> IO CInt

foreign import ccall "archive.h archive_write_set_format_mtree_classic"
  archive_write_set_format_mtree_classic :: Ptr (Archive W) -> IO CInt

foreign import ccall "archive.h archive_write_set_format_pax"
  archive_write_set_format_pax :: Ptr (Archive W) -> IO CInt

foreign import ccall "archive.h archive_write_set_format_pax_restricted"
  archive_write_set_format_pax_restricted :: Ptr (Archive W) -> IO CInt

foreign import ccall "archive.h archive_write_set_format_raw"
  archive_write_set_format_raw :: Ptr (Archive W) -> IO CInt

foreign import ccall "archive.h archive_write_set_format_shar"
  archive_write_set_format_shar :: Ptr (Archive W) -> IO CInt

foreign import ccall "archive.h archive_write_set_format_shar_dump"
  archive_write_set_format_shar_dump :: Ptr (Archive W) -> IO CInt

foreign import ccall "archive.h archive_write_set_format_ustar"
  archive_write_set_format_ustar :: Ptr (Archive W) -> IO CInt

foreign import ccall "archive.h archive_write_set_format_v7tar"
  archive_write_set_format_v7tar :: Ptr (Archive W) -> IO CInt

foreign import ccall "archive.h archive_write_set_format_warc"
  archive_write_set_format_warc :: Ptr (Archive W) -> IO CInt

foreign import ccall "archive.h archive_write_set_format_xar"
  archive_write_set_format_xar :: Ptr (Archive W) -> IO CInt

foreign import ccall "archive.h archive_write_set_format_zip"
  archive_write_set_format_zip :: Ptr (Archive W) -> IO CInt


setWriteFormat :: Ptr (Archive W) -> Format -> IO ()
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


addWriteFilter :: Ptr (Archive W) -> Filter -> IO ()
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
      FilterLzma -> archive_write_add_filter_lzma p
      FilterLzop -> archive_write_add_filter_lzop p
      FilterNone -> archive_write_add_filter_none p
      FilterUuencode -> archive_write_add_filter_uuencode p
      FilterXz -> archive_write_add_filter_xz p


setWriteFilters :: Ptr (Archive W) -> [Filter] -> IO ()
setWriteFilters p [] = addWriteFilter p FilterNone
setWriteFilters p fs = mapM_ (addWriteFilter p) fs


withArchiveWrite :: Format -> [Filter] -> (Ptr (Archive W) -> IO a) -> IO a
withArchiveWrite format filters go =
  bracket archive_write_new archive_free $ \ar -> do
    setWriteFormat ar format
    setWriteFilters ar filters
    go ar


foreign import ccall "archive.h archive_write_header"
  archive_write_header :: Ptr (Archive W) -> Ptr Entry -> IO CInt

archiveWriteEntry :: Ptr (Archive W) -> Entry -> IO ()
archiveWriteEntry ar entry =
  withEntry $ \en -> do
    pokeEntry en entry
    archive_write_header ar en >>= checkArchiveError_ ar


foreign import ccall "archive.h archive_write_data"
  archive_write_data :: Ptr (Archive W) -> Ptr () -> CSize -> IO CSize

archiveWriteData :: Ptr (Archive W) -> ByteString -> IO ()
archiveWriteData ar _bytes =
  B.useAsCStringLen _bytes $ \(castPtr -> _bytes, fromIntegral -> len) ->
    archive_write_data ar _bytes len >>= checkArchiveWriteError ar
