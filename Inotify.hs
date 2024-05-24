{-# LANGUAGE CApiFFI #-}

module Inotify where

import Control.Concurrent (threadWaitRead)
import Control.Monad (forever)
import Data.ByteString.Short (ShortByteString)
import Data.ByteString.Short qualified as ByteString.Short
import Data.ByteString.Short.Internal qualified as ByteString.Short (createFromPtr)
import Data.IORef (IORef, newIORef, readIORef, writeIORef)
import Data.Word (Word32)
import Foreign (Ptr, Storable (peek, sizeOf), allocaBytesAligned, minusPtr, plusPtr)
import Foreign.C (CChar, CInt (..), CSize (..), Errno (..), eAGAIN, getErrno)
import Foreign.C.ConstPtr (ConstPtr (ConstPtr))
import System.OsString (OsString, unsafeEncodeUtf)
import System.OsString.Internal.Types (OsString (..), PosixString (..))
import System.Posix.Types (CSsize (..), Fd (..))
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
    with \inotify -> do
      putStrLn "make an inotify!"

      putStrLn "adding a watch!"

      watch inotify (unsafeEncodeUtf ".") _IN_ALL_EVENTS >>= print

      putStrLn "awaiting events!"
      forever @IO @() @() do
        await inotify >>= print
  print result

with :: (Inotify -> IO a) -> IO (Either CInt a)
with action =
  inotify_init1 _IN_NONBLOCK >>= \case
    -1 -> do
      Errno errno <- getErrno
      pure (Left errno)
    fd -> do
      offsetRef <- newIORef 0
      buflenRef <- newIORef 0
      allocaBytesAligned 4096 (sizeOf (0 :: Int)) \buffer ->
        Right <$> action Inotify {fd, buffer, offsetRef, buflenRef}

watch :: Inotify -> OsString -> Events -> IO (Either CInt WatchDescriptor)
watch inotify (OsString (PosixString path)) mask =
  ByteString.Short.useAsCString path \cpath ->
    inotify_add_watch inotify.fd (ConstPtr cpath) mask >>= \case
      -1 -> do
        Errno errno <- getErrno
        pure (Left errno)
      wd -> pure (Right wd)

data Event
  = Event
  { wd :: {-# UNPACK #-} !CInt,
    mask :: {-# UNPACK #-} !Word32,
    cookie :: {-# UNPACK #-} !Word32,
    len :: {-# UNPACK #-} !Word32,
    name :: !ShortByteString
  }
  deriving stock (Show)

await :: Inotify -> IO (Either CInt Event)
await inotify = do
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

parseEvent :: Inotify -> Int -> IO Event
parseEvent inotify offset = do
  wd <- peek @CInt (plusPtr inotify.buffer offset)
  mask <- peek @Word32 (plusPtr inotify.buffer (offset + sizeOfCInt))
  cookie <- peek @Word32 (plusPtr inotify.buffer (offset + sizeOfCInt + 4))
  len <- peek @Word32 (plusPtr inotify.buffer (offset + sizeOfCInt + 8))
  name <-
    if len == 0
      then pure ByteString.Short.empty
      else do
        let beginning = plusPtr inotify.buffer (offset + sizeOfCInt + 12)
        end <- rawmemchr (ConstPtr beginning) 0
        ByteString.Short.createFromPtr beginning (minusPtr end beginning)
  writeIORef inotify.offsetRef $! offset + sizeOfCInt + 12 + fromIntegral @Word32 @Int len
  pure (Event {wd, mask, cookie, len, name})

sizeOfCInt :: Int
sizeOfCInt =
  sizeOf (0 :: CInt)

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

foreign import capi unsafe "string.h rawmemchr"
  rawmemchr :: ConstPtr a -> CInt -> IO (Ptr b)

foreign import capi unsafe "unistd.h read"
  read :: Fd -> Ptr a -> CSize -> IO CSsize

foreign import capi unsafe "sys/inotify.h inotify_init1"
  inotify_init1 :: CInt -> IO Fd

foreign import capi unsafe "sys/inotify.h inotify_add_watch"
  inotify_add_watch :: Fd -> ConstPtr CChar -> Word32 -> IO CInt

foreign import capi "sys/inotify.h value IN_ALL_EVENTS" _IN_ALL_EVENTS :: Word32

foreign import capi "sys/inotify.h value IN_NONBLOCK" _IN_NONBLOCK :: CInt
