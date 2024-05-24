{-# LANGUAGE CApiFFI #-}

module Inotify where

import Control.Concurrent
import Control.Monad
import Data.ByteString.Short (ShortByteString)
import Data.ByteString.Short qualified as ByteString.Short
import Data.ByteString.Short.Internal qualified as ByteString.Short (createFromPtr)
import Data.IORef
import Data.Word
import Foreign
import Foreign.C
import Foreign.C.ConstPtr
import System.OsString
import System.OsString.Internal.Types (OsString (..), PosixString (..))
import System.Posix.Types
import Prelude hiding (read)

type InitFlags = CInt

type Events = Word32

type WatchDescriptor = CInt

data Inotify
  = Inotify
  { fd :: {-# UNPACK #-} !Fd,
    buffer :: {-# UNPACK #-} !(Ptr ()),
    offsetRef :: {-# UNPACK #-} !(IORef Int),
    buflenRef :: {-# UNPACK #-} !(IORef Int)
  }

main :: IO ()
main = do
  result <-
    withInotify \inotify -> do
      putStrLn "make an inotify!"

      putStrLn "adding a watch!"

      inotifyAddWatch inotify.fd (unsafeEncodeUtf ".") _IN_ALL_EVENTS >>= print

      putStrLn "awaiting events!"
      forever @IO @() @() do
        awaitInotifyEvent inotify >>= print
  print result

withInotify :: (Inotify -> IO a) -> IO (Either CInt a)
withInotify action =
  inotify_init1 _IN_NONBLOCK >>= \case
    -1 -> do
      Errno errno <- getErrno
      pure (Left errno)
    fd -> do
      offsetRef <- newIORef 0
      buflenRef <- newIORef 0
      allocaBytesAligned 4096 (sizeOf (0 :: Int)) \buffer ->
        Right <$> action Inotify {fd, buffer, offsetRef, buflenRef}

inotifyAddWatch :: Fd -> OsString -> Events -> IO (Either CInt WatchDescriptor)
inotifyAddWatch fd (OsString (PosixString path)) mask =
  ByteString.Short.useAsCString path \cpath ->
    inotify_add_watch fd (ConstPtr cpath) mask >>= \case
      -1 -> do
        Errno errno <- getErrno
        pure (Left errno)
      wd -> pure (Right wd)

data InotifyEvent
  = InotifyEvent
  { wd :: CInt,
    mask :: Word32,
    cookie :: Word32,
    len :: Word32
  }
  deriving stock (Show)

awaitInotifyEvent :: Inotify -> IO (Either CInt InotifyEvent)
awaitInotifyEvent inotify = do
  offset <- readIORef inotify.offsetRef
  buflen <- readIORef inotify.buflenRef
  if offset >= buflen
    then do
      readloop inotify.fd inotify.buffer 4096 >>= \case
        Left (Errno errno) -> pure (Left errno)
        Right len -> do
          writeIORef inotify.offsetRef 0
          writeIORef inotify.buflenRef (fromIntegral @CSsize @Int len)
          Right <$> parseEvent inotify 0
    else Right <$> parseEvent inotify offset

parseEvent :: Inotify -> Int -> IO InotifyEvent
parseEvent inotify offset = do
  wd <- peek @CInt (plusPtr inotify.buffer offset)
  mask <- peek @Word32 (plusPtr inotify.buffer (offset + sizeOfCInt))
  cookie <- peek @Word32 (plusPtr inotify.buffer (offset + sizeOfCInt + 4))
  len <- peek @Word32 (plusPtr inotify.buffer (offset + sizeOfCInt + 8))
  writeIORef inotify.offsetRef $! offset + sizeOfCInt + 12 + fromIntegral @Word32 @Int len
  pure (InotifyEvent {wd, mask, cookie, len})

sizeOfCInt :: Int
sizeOfCInt =
  sizeOf (0 :: CInt)

-- struct inotify_event {
--     int      wd;       /* Watch descriptor */
--     uint32_t mask;     /* Mask describing event */
--     uint32_t cookie;   /* Unique cookie associating related
--                           events (for rename(2)) */
--     uint32_t len;      /* Size of name field */
--     char     name[];   /* Optional null-terminated name */
-- };

inotifyGetEvents :: Fd -> IO (Either Errno ShortByteString)
inotifyGetEvents fd = do
  allocaBytesAligned 4096 (sizeOf (0 :: Int)) \buf -> do
    readloop fd buf 4096 >>= \case
      Left errno -> pure (Left errno)
      Right len -> Right <$> ByteString.Short.createFromPtr buf (fromIntegral @CSsize @Int len)

readloop :: Fd -> Ptr a -> CSize -> IO (Either Errno CSsize)
readloop fd buf size =
  read fd buf size >>= \case
    -1 -> do
      errno <- getErrno
      if errno == eAGAIN
        then do
          threadWaitRead fd
          readloop fd buf size
        else pure (Left errno)
    len -> pure (Right len)

foreign import capi unsafe "unistd.h read"
  read :: Fd -> Ptr a -> CSize -> IO CSsize

foreign import capi unsafe "sys/inotify.h inotify_init1"
  inotify_init1 :: CInt -> IO Fd

foreign import capi unsafe "sys/inotify.h inotify_add_watch"
  inotify_add_watch :: Fd -> ConstPtr CChar -> Word32 -> IO CInt

foreign import capi "sys/inotify.h value IN_ALL_EVENTS" _IN_ALL_EVENTS :: Word32

foreign import capi "sys/inotify.h value IN_NONBLOCK" _IN_NONBLOCK :: CInt
