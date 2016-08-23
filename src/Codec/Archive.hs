{-# LANGUAGE RecordWildCards #-}

module Codec.Archive
       ( Event(..), readArchive, writeArchive
       ) where

import Data.ByteString ( ByteString )
import System.IO.Streams
       ( InputStream, OutputStream, makeInputStream, makeOutputStream, unRead )

import Codec.Archive.Internal.Read
import Codec.Archive.Internal.Write


-- | 'Event' is an event encountered while reading an archive, or an event
-- produced while writing an archive. Files in an archive are represented by
-- a file header followed by a sequence of data blocks. Reading an archive
-- produces a stream of files so represented. Writing an archive consumes a
-- stream of the same.
data Event = E Entry  -- ^ a file header
           | B ByteString  -- ^ a block of data


-- | Stream an archive from an open file descriptor. The file remains open
-- after this function returns.
readArchive :: Fd -> (InputStream Event -> IO a) -> IO a
readArchive fd go =
  withArchiveRead $ \ar -> do
    archiveReadOpenFd ar fd blocksize
    let producer = do
          dat <- archiveReadData ar
          case dat of
            Nothing -> fmap E <$> archiveReadEntry ar
            Just _ -> fmap B <$> pure dat
    first <- archiveReadEntry ar
    stream <- case first of
                Nothing -> makeInputStream (pure Nothing)
                Just entry -> do
                  s <- makeInputStream producer
                  unRead (E entry) s
                  pure s
    go stream
  where
    blocksize = 4096


-- | Stream an archive to an open file descriptor. The file remains open after
-- this function returns.
writeArchive :: Fd -> Format -> [Filter] -> (OutputStream Event -> IO a) -> IO a
writeArchive fd format filters go =
  withArchiveWrite format filters $ \ar -> do
    archiveWriteOpenFd ar fd
    let consumer Nothing = pure ()
        consumer (Just ev) =
          case ev of
            E entry -> archiveWriteEntry ar entry
            B block -> archiveWriteData ar block
    makeOutputStream consumer >>= go
