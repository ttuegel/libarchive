{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE RecordWildCards #-}

module Codec.Archive where

import Codec.Archive.Internal
import Control.Exception ( Exception, bracket, throwIO )
import Control.Monad ( when )
import Data.ByteString ( ByteString )
import qualified Data.ByteString as B
import Data.Int ( Int64 )
import Data.Typeable
import Foreign.C.String
import Foreign.C.Types ( CInt, CSize )
import Foreign.Marshal.Alloc ( free, mallocBytes )
import Foreign.Ptr ( Ptr, castPtr )
import System.IO.Streams ( InputStream, makeInputStream, unRead )
import System.Posix.Types
       ( DeviceID, EpochTime, Fd, FileMode, GroupID, LinkCount, UserID )


data ArchiveException = ArchiveException CInt
  deriving ( Show, Typeable )

instance Exception ArchiveException


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

getEntry :: Ptr ArchiveEntry -> IO Entry
getEntry p = do
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
