{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE RecordWildCards #-}

module Codec.Archive where

import Codec.Archive.Internal
import Control.Exception ( Exception, bracket, throwIO )
import Control.Monad ( when )
import Data.ByteString ( ByteString )
import qualified Data.ByteString as B
import Data.Typeable
import Foreign.C.String
import Foreign.C.Types ( CInt, CSize )
import Foreign.Marshal.Alloc ( free, mallocBytes )
import Foreign.Ptr ( Ptr, castPtr )
import System.IO.Streams ( InputStream, makeInputStream, unRead )
import System.Posix.Types ( Fd, FileMode )


data ArchiveException = ArchiveException CInt
  deriving ( Show, Typeable )

instance Exception ArchiveException


data Entry = Entry { hardlink :: FilePath
                   , pathname :: FilePath
                   , sourcepath :: FilePath
                   , symlink :: FilePath
                   , filetype :: FileMode
                   , mode :: FileMode
                   }

getEntry :: Ptr ArchiveEntry -> IO Entry
getEntry p = do
  hardlink <- archive_entry_hardlink_w p >>= peekCWString
  pathname <- archive_entry_pathname_w p >>= peekCWString
  sourcepath <- archive_entry_sourcepath_w p >>= peekCWString
  symlink <- archive_entry_symlink_w p >>= peekCWString
  mode <- archive_entry_mode p
  filetype <- archive_entry_filetype p
  pure Entry {..}


data Event = E Entry | B ByteString


archiveBufferSize :: CSize
archiveBufferSize = 4096


readArchive :: Fd -> (InputStream Event -> IO a) -> IO a
readArchive fd go = bracket before after during
  where
    before = do
      pa <- archive_read_new
      _err <- archive_read_open_fd pa fd archiveBufferSize
      when (_err < archiveOK) (throwIO $ ArchiveException _err)
      _err <- archive_read_support_filter_all pa
      when (_err < archiveOK) (throwIO $ ArchiveException _err)
      pe <- archive_entry_new
      buf <- mallocBytes (fromIntegral archiveBufferSize)
      pure (pa, pe, buf)

    after (pa, pe, buf) = archive_entry_free pe >> archive_free pa >> free buf

    during (pa, pe, buf) = do
      let next = do
            _err <- archive_read_next_header2 pa pe
            if _err < archiveOK
              then throwIO $ ArchiveException _err
              else if _err == archiveEOF
              then pure Nothing
              else Just . E <$> getEntry pe

      first <- next

      let streamer = do
            len <- archive_read_data pa buf archiveBufferSize
            if len > 0
              then Just . B <$> B.packCStringLen (castPtr buf, fromIntegral len)
              else next

      stream <- case first of
                  Nothing -> makeInputStream (pure Nothing)
                  Just ev -> do
                    s <- makeInputStream streamer
                    unRead ev s
                    pure s
      go stream
