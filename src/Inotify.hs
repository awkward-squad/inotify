{-# LANGUAGE CApiFFI #-}
{-# LANGUAGE MagicHash #-}
{-# LANGUAGE UnliftedFFITypes #-}

module Inotify
  ( Instance,
    Event (..),
    with,
    watch,
    unwatch,
    await,
    poll,

    -- * Event types
    pattern Access,
    pattern Attrib,
    pattern CloseNowrite,
    pattern CloseWrite,
    pattern Create,
    pattern Delete,
    pattern DeleteSelf,
    pattern Modify,
    pattern MoveSelf,
    pattern MovedFrom,
    pattern MovedTo,
    pattern Open,

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

    -- * Event information
    ignored,
    isdir,
    qOverflow,
    unmount,
  )
where

import Control.Concurrent (threadWaitRead)
import Control.Exception qualified as Exception
import Control.Monad (forever)
import Data.Bits ((.&.), (.|.))
import Data.ByteString.Short (ShortByteString)
import Data.ByteString.Short qualified as ByteString.Short
import Data.ByteString.Short.Internal qualified as ByteString.Short (createFromPtr)
import Data.Coerce (coerce)
import Data.IORef (IORef, newIORef, readIORef, writeIORef)
import Data.List qualified as List
import Data.Word (Word32)
import Foreign (Ptr, Storable (peek, sizeOf), allocaBytesAligned, minusPtr, plusPtr)
import Foreign.C (CInt (..), CSize (..), Errno (..), eAGAIN, getErrno)
import Foreign.C.ConstPtr (ConstPtr (ConstPtr))
import Posix.Inotify.Bindings
import System.OsString (OsString, unsafeEncodeUtf)
import System.OsString.Internal.Types (OsString (..), PosixString (..))
import System.Posix.Types (CSsize (..), Fd (..))
import Prelude hiding (read)

type WatchDescriptor = CInt

data Instance
  = Instance
  { fd :: {-# UNPACK #-} !CInt,
    buffer :: {-# UNPACK #-} !(Ptr ()),
    offsetRef :: {-# UNPACK #-} !(IORef Int),
    buflenRef :: {-# UNPACK #-} !(IORef Int)
  }

newtype Mask
  = Mask Word32
  deriving newtype (Show)
  deriving (Semigroup) via (Or)

newtype Option
  = Option Word32
  deriving (Semigroup) via (Or)

newtype Or
  = Or Word32

instance Semigroup Or where
  Or x <> Or y =
    Or (x .|. y)

main :: IO (Either CInt ())
main = do
  with \inotify -> do
    watch inotify (unsafeEncodeUtf ".") [allEvents] [] >>= print
    forever @IO @() @() do
      await inotify >>= print

-- | Perform an action with a new inotify instance.
with :: (Instance -> IO a) -> IO (Either CInt a)
with action =
  Exception.mask \unmask -> do
    -- Initialize an inotify instance with IN_CLOEXEC (to prevent leaking file descriptors on fork, even though no one
    -- really forks in Haskell) and IN_NONBLOCK (because we want to perform nonblocking reads).
    inotify_init1 (_IN_CLOEXEC .|. _IN_NONBLOCK) >>= \case
      -1 -> do
        Errno errno <- getErrno
        pure (Left errno)
      fd -> do
        (`Exception.finally` c_close fd) do
          unmask do
            offsetRef <- newIORef 0
            buflenRef <- newIORef 0
            allocaBytesAligned 4096 (sizeOf (0 :: Int)) \buffer ->
              Right <$> action Instance {fd, buffer, offsetRef, buflenRef}

-- | Add a new watch, or modify an existing watch.
watch :: Instance -> OsString -> [Mask] -> [Option] -> IO (Either CInt WatchDescriptor)
watch inst (OsString (PosixString path)) mask opts =
  ByteString.Short.useAsCString path \cpath ->
    inotify_add_watch inst.fd (ConstPtr cpath) (combine mask opts) >>= \case
      -1 -> do
        Errno errno <- getErrno
        pure (Left errno)
      wd -> pure (Right wd)

combine :: [Mask] -> [Option] -> Word32
combine masks options =
  List.foldl'
    (.|.)
    (List.foldl' (.|.) 0 (coerce @[Mask] @[Word32] masks))
    (coerce @[Option] @[Word32] options)

-- | Remove a watch.
unwatch :: Instance -> WatchDescriptor -> IO (Either CInt ())
unwatch inst wd =
  inotify_rm_watch inst.fd wd >>= \case
    0 -> pure (Right ())
    _ -> do
      Errno errno <- getErrno
      pure (Left errno)

-- | Await an event.
await :: Instance -> IO (Either CInt Event)
await inotify = do
  offset <- readIORef inotify.offsetRef
  buflen <- readIORef inotify.buflenRef
  if offset >= buflen
    then do
      readloop inotify.fd inotify.buffer 4096 >>= \case
        Left errno -> pure (Left errno)
        Right len -> do
          writeIORef inotify.offsetRef 0
          writeIORef inotify.buflenRef (fromIntegral @CSsize @Int len)
          Right <$> parseEvent inotify 0
    else Right <$> parseEvent inotify offset

-- | Poll for an event.
poll :: Instance -> IO (Either CInt (Maybe Event))
poll inst = do
  offset <- readIORef inst.offsetRef
  buflen <- readIORef inst.buflenRef
  if offset >= buflen
    then do
      c_read inst.fd inst.buffer 4096 >>= \case
        -1 -> do
          Errno errno <- getErrno
          pure
            if Errno errno == eAGAIN
              then Right Nothing
              else Left errno
        len -> do
          writeIORef inst.offsetRef 0
          writeIORef inst.buflenRef (fromIntegral @CSsize @Int len)
          Right . Just <$> parseEvent inst 0
    else Right . Just <$> parseEvent inst offset

parseEvent :: Instance -> Int -> IO Event
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
        end <- c_rawmemchr (ConstPtr beginning) 0
        ByteString.Short.createFromPtr beginning (minusPtr end beginning)
  writeIORef inotify.offsetRef $! offset + sizeOfCInt + 12 + fromIntegral @Word32 @Int len
  pure
    Event
      { wd,
        mask = Mask mask,
        cookie,
        name
      }

has :: Word32 -> Mask -> Bool
has bit (Mask set) =
  set .&. bit /= 0

readloop :: CInt -> Ptr a -> CSize -> IO (Either CInt CSsize)
readloop fd buf size =
  c_read fd buf size >>= \case
    -1 -> do
      Errno errno <- getErrno
      if Errno errno == eAGAIN
        then do
          threadWaitRead (Fd fd)
          readloop fd buf size
        else pure (Left errno)
    len -> pure (Right len)

data Event
  = Event
  { wd :: {-# UNPACK #-} !CInt,
    mask :: {-# UNPACK #-} !Mask,
    cookie :: {-# UNPACK #-} !Word32,
    name :: !ShortByteString
  }
  deriving stock (Show)

-- | A file was accessed.
pattern Access :: Mask
pattern Access <- (has _IN_ACCESS -> True)
  where
    Access = Mask _IN_ACCESS

-- | A file's metadata changed.
pattern Attrib :: Mask
pattern Attrib <- (has _IN_ATTRIB -> True)
  where
    Attrib = Mask _IN_ATTRIB

-- | A file or directory not opened for writing was closed.
pattern CloseNowrite :: Mask
pattern CloseNowrite <- (has _IN_CLOSE_NOWRITE -> True)
  where
    CloseNowrite = Mask _IN_CLOSE_NOWRITE

-- | A file opened for writing was closed.
pattern CloseWrite :: Mask
pattern CloseWrite <- (has _IN_CLOSE_WRITE -> True)
  where
    CloseWrite = Mask _IN_CLOSE_WRITE

-- | A file or directory was created.
pattern Create :: Mask
pattern Create <- (has _IN_CREATE -> True)
  where
    Create = Mask _IN_CREATE

-- | A file or directory was deleted.
pattern Delete :: Mask
pattern Delete <- (has _IN_DELETE -> True)
  where
    Delete = Mask _IN_DELETE

-- | A watched file or directory itself was deleted.
pattern DeleteSelf :: Mask
pattern DeleteSelf <- (has _IN_DELETE_SELF -> True)
  where
    DeleteSelf = Mask _IN_DELETE_SELF

-- | A file was modified.
pattern Modify :: Mask
pattern Modify <- (has _IN_MODIFY -> True)
  where
    Modify = Mask _IN_MODIFY

-- | A watched file or directory itself was moved.
pattern MoveSelf :: Mask
pattern MoveSelf <- (has _IN_MOVE_SELF -> True)
  where
    MoveSelf = Mask _IN_MOVE_SELF

-- | A file was moved from a directory.
pattern MovedFrom :: Mask
pattern MovedFrom <- (has _IN_MOVED_FROM -> True)
  where
    MovedFrom = Mask _IN_MOVED_FROM

-- | A file was moved to a directory.
pattern MovedTo :: Mask
pattern MovedTo <- (has _IN_MOVED_TO -> True)
  where
    MovedTo = Mask _IN_MOVED_TO

-- | A file or directory was opened.
pattern Open :: Mask
pattern Open <- (has _IN_OPEN -> True)
  where
    Open = Mask _IN_OPEN

-- | A watch was removed explicitly or automatically.
ignored :: Mask -> Bool
ignored =
  has _IN_IGNORED

-- | The subject of this event is a directory.
isdir :: Mask -> Bool
isdir =
  has _IN_ISDIR

-- | The event queue overflowed.
qOverflow :: Mask -> Bool
qOverflow =
  has _IN_Q_OVERFLOW

-- | The filesystem contained the watched object was unmounted.
unmount :: Mask -> Bool
unmount =
  has _IN_UNMOUNT

allEvents :: Mask
allEvents = Mask _IN_ALL_EVENTS

close :: Mask
close = Mask _IN_CLOSE

move :: Mask
move = Mask _IN_MOVE

-- | Don't dereference the path if it's a symbolic link.
dontFollow :: Option
dontFollow = Option _IN_DONT_FOLLOW

-- | Don't generate events for children of a watched directory after they have been unlinked.
exclUnlink :: Option
exclUnlink = Option _IN_EXCL_UNLINK

-- | If the path is already being watched, add to the watch rather than replace it.
maskAdd :: Option
maskAdd = Option _IN_MASK_ADD

-- | Error if the path is not already being watched.
maskCreate :: Option
maskCreate = Option _IN_MASK_CREATE

-- | Stop watching the path after one event.
oneshot :: Option
oneshot = Option _IN_ONESHOT

-- | Error if the path is not a directory.
onlydir :: Option
onlydir = Option _IN_ONLYDIR

sizeOfCInt :: Int
sizeOfCInt =
  sizeOf (0 :: CInt)

foreign import capi unsafe "unistd.h close"
  c_close :: CInt -> IO CInt

foreign import capi unsafe "string.h rawmemchr"
  c_rawmemchr :: ConstPtr a -> CInt -> IO (Ptr b)

foreign import capi unsafe "unistd.h read"
  c_read :: CInt -> Ptr a -> CSize -> IO CSsize
