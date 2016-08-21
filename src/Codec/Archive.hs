{-# LANGUAGE RecordWildCards #-}

module Codec.Archive where

import Codec.Archive.Read
import Codec.Archive.Write
import Data.ByteString ( ByteString )
import System.IO.Streams
       ( InputStream, OutputStream, makeInputStream, makeOutputStream, unRead )


data Event = E Entry | B ByteString


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
