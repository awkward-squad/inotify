{-# LANGUAGE CApiFFI #-}

module Inotify
  ( Inotify,
    with,
    watch,
    Event (..),
    EventType (..),
    await,

    -- * Event types
    access,
    attrib,
    closeNowrite,
    closeWrite,
    create,
    delete,
    deleteSelf,
    modify,
    moveSelf,
    movedFrom,
    movedTo,
    open,

    -- ** Derived event types
    allEvents,
    close,
    move,

    -- * Watch options
    dontFollow,
    exclUnlink,
    maskAdd,
    maskCreate,
    oneshot,
    onlydir,
  )
where

import Control.Concurrent (threadWaitRead)
import Control.Monad (forever)
import Data.Bits ((.&.))
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

type WatchDescriptor = CInt

data Inotify
  = Inotify
  { fd :: {-# UNPACK #-} !Fd,
    buffer :: {-# UNPACK #-} !(Ptr ()),
    offsetRef :: {-# UNPACK #-} !(IORef Int),
    buflenRef :: {-# UNPACK #-} !(IORef Int)
  }

main :: IO (Either CInt ())
main = do
  with \inotify -> do
    watch inotify (unsafeEncodeUtf ".") allEvents >>= print
    putStrLn "awaiting events!"
    forever @IO @() @() do
      await inotify >>= print

with :: (Inotify -> IO a) -> IO (Either CInt a)
with action =
  inotify_init1 nonblock >>= \case
    -1 -> do
      Errno errno <- getErrno
      pure (Left errno)
    fd -> do
      offsetRef <- newIORef 0
      buflenRef <- newIORef 0
      allocaBytesAligned 4096 (sizeOf (0 :: Int)) \buffer ->
        Right <$> action Inotify {fd, buffer, offsetRef, buflenRef}

watch :: Inotify -> OsString -> Word32 -> IO (Either CInt WatchDescriptor)
watch inotify (OsString (PosixString path)) mask =
  ByteString.Short.useAsCString path \cpath ->
    inotify_add_watch inotify.fd (ConstPtr cpath) mask >>= \case
      -1 -> do
        Errno errno <- getErrno
        pure (Left errno)
      wd -> pure (Right wd)

data Event
  = Event
  { type_ :: !EventType,
    wd :: {-# UNPACK #-} !CInt,
    name :: !ShortByteString,
    ignored :: !Bool,
    isdir :: !Bool,
    qOverflow :: !Bool,
    unmount :: !Bool
  }
  deriving stock (Show)

data EventType
  = Access
  | Attrib
  | CloseNowrite
  | CloseWrite
  | Create
  | Delete
  | DeleteSelf
  | Modify
  | MoveSelf
  | MovedFrom !Word32
  | MovedTo !Word32
  | Open
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
  pure
    Event
      { type_ =
          if
            | has mask access -> Access
            | has mask attrib -> Attrib
            | has mask closeNowrite -> CloseNowrite
            | has mask closeWrite -> CloseWrite
            | has mask create -> Create
            | has mask delete -> Delete
            | has mask deleteSelf -> DeleteSelf
            | has mask modify -> Modify
            | has mask moveSelf -> MoveSelf
            | has mask movedFrom -> MovedFrom cookie
            | has mask movedTo -> MovedTo cookie
            | has mask open -> Open
            | otherwise -> undefined,
        wd,
        name,
        ignored = has mask _IN_IGNORED,
        isdir = has mask _IN_ISDIR,
        qOverflow = has mask _IN_Q_OVERFLOW,
        unmount = has mask _IN_UNMOUNT
      }

has :: Word32 -> Word32 -> Bool
has set bit =
  set .&. bit /= 0

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

foreign import capi "sys/inotify.h value IN_ACCESS" access :: Word32

foreign import capi "sys/inotify.h value IN_ALL_EVENTS" allEvents :: Word32

foreign import capi "sys/inotify.h value IN_ATTRIB" attrib :: Word32

foreign import capi "sys/inotify.h value IN_CLOSE" close :: Word32

foreign import capi "sys/inotify.h value IN_CLOSE_NOWRITE" closeNowrite :: Word32

foreign import capi "sys/inotify.h value IN_CLOSE_WRITE" closeWrite :: Word32

foreign import capi "sys/inotify.h value IN_CREATE" create :: Word32

foreign import capi "sys/inotify.h value IN_DELETE" delete :: Word32

foreign import capi "sys/inotify.h value IN_DELETE_SELF" deleteSelf :: Word32

foreign import capi "sys/inotify.h value IN_DONT_FOLLOW" dontFollow :: Word32

foreign import capi "sys/inotify.h value IN_EXCL_UNLINK" exclUnlink :: Word32

foreign import capi "sys/inotify.h value IN_IGNORED" _IN_IGNORED :: Word32

foreign import capi "sys/inotify.h value IN_ISDIR" _IN_ISDIR :: Word32

foreign import capi "sys/inotify.h value IN_MASK_ADD" maskAdd :: Word32

foreign import capi "sys/inotify.h value IN_MASK_CREATE" maskCreate :: Word32

foreign import capi "sys/inotify.h value IN_MODIFY" modify :: Word32

foreign import capi "sys/inotify.h value IN_MOVE" move :: Word32

foreign import capi "sys/inotify.h value IN_MOVED_FROM" movedFrom :: Word32

foreign import capi "sys/inotify.h value IN_MOVED_TO" movedTo :: Word32

foreign import capi "sys/inotify.h value IN_MOVE_SELF" moveSelf :: Word32

foreign import capi "sys/inotify.h value IN_NONBLOCK" nonblock :: CInt

foreign import capi "sys/inotify.h value IN_ONESHOT" oneshot :: Word32

foreign import capi "sys/inotify.h value IN_ONLYDIR" onlydir :: Word32

foreign import capi "sys/inotify.h value IN_OPEN" open :: Word32

foreign import capi "sys/inotify.h value IN_Q_OVERFLOW" _IN_Q_OVERFLOW :: Word32

foreign import capi "sys/inotify.h value IN_UNMOUNT" _IN_UNMOUNT :: Word32
