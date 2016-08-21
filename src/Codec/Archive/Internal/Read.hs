{-# LANGUAGE ForeignFunctionInterface #-}
{-# LANGUAGE RecordWildCards #-}

module Codec.Archive.Internal.Read
       ( withArchiveRead, archiveReadOpenFd
       , archiveReadEntry, archiveReadDataInto, archiveReadData
         -- * Courtesy exports
       , module Codec.Archive.Internal.Entry
       , module Codec.Archive.Internal.Error
       , module Codec.Archive.Internal.Types
       , Fd
       ) where

import Control.Exception
import Data.ByteString ( ByteString )
import qualified Data.ByteString as B
import Data.Int
import Foreign.C.Error
import Foreign.C.Types
import Foreign.Marshal.Alloc ( free, mallocBytes )
import Foreign.Ptr ( Ptr, castPtr )
import System.Posix.Types

import Codec.Archive.Internal.Entry
import Codec.Archive.Internal.Error
import Codec.Archive.Internal.Types


foreign import ccall "archive.h archive_free"
  archive_free :: Ptr (Archive rw) -> IO CInt

foreign import ccall "archive.h archive_read_new"
  archive_read_new :: IO (Ptr (Archive R))

foreign import ccall "archive.h archive_read_support_filter_all"
  archive_read_support_filter_all :: Ptr (Archive R) -> IO CInt

withArchiveRead :: (Ptr (Archive R) -> IO a) -> IO a
withArchiveRead go =
  bracket archive_read_new archive_free $ \ar -> do
    archive_read_support_filter_all ar >>= checkArchiveError_ ar
    go ar


foreign import ccall "archive.h archive_read_open_fd"
  archive_read_open_fd :: Ptr (Archive R) -> Fd -> CSize -> IO CInt

archiveReadOpenFd :: Ptr (Archive R) -> Fd -> CSize -> IO ()
archiveReadOpenFd ar fd blocksize =
  archive_read_open_fd ar fd blocksize >>= checkArchiveError_ ar


foreign import ccall "archive.h archive_read_data"
  archive_read_data :: Ptr (Archive R) -> Ptr () -> CSize -> IO CSize

foreign import ccall "archive.h archive_read_next_header2"
  archive_read_next_header2 :: Ptr (Archive R) -> Ptr Entry -> IO CInt


archiveReadEntry :: Ptr (Archive R) -> IO (Maybe Entry)
archiveReadEntry ar =
  withEntry $ \en -> do
    eof <- archive_read_next_header2 ar en >>= checkArchiveError ar
    if eof then pure Nothing else Just <$> peekEntry ar en


archiveReadDataInto :: Ptr (Archive R) -> Ptr () -> CSize -> IO Bool
archiveReadDataInto ar buf bufsz = do
  sz <- archive_read_data ar buf bufsz
  checkArchiveReadError ar sz


archiveBufferSize :: Int
archiveBufferSize = 4096


archiveReadData :: Ptr (Archive R) -> IO (Maybe ByteString)
archiveReadData ar =
  bracket (mallocBytes archiveBufferSize) free $ \buf -> do
    let size = fromIntegral archiveBufferSize
        len = fromIntegral archiveBufferSize
    eof <- archiveReadDataInto ar buf size
    if eof then pure Nothing else Just <$> B.packCStringLen (castPtr buf, len)
