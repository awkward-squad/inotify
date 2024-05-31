{-# LANGUAGE CApiFFI #-}

module Inotify
  ( Inotify,
    Event (..),
    Mask,
    Option,
    WatchDescriptor,
    Error (..),
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

    -- ** Meta event types
    pattern Ignored,
    pattern QOverflow,
    pattern Unmount,

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
    isdir,

    -- * Helpful debugging view of events
    showEvent,
  )
where

import Control.Concurrent (threadWaitRead)
import Control.Exception qualified as Exception
import Data.Bits ((.&.), (.|.))
import Data.ByteString.Short qualified as ByteString.Short
import Data.ByteString.Short.Internal qualified as ByteString.Short (createFromPtr)
import Data.Coerce (coerce)
import Data.Either
import Data.Functor (void)
import Data.IORef (IORef, newIORef, readIORef, writeIORef)
import Data.List qualified as List
import Data.Word (Word32)
import Foreign (Ptr, Storable (peek, sizeOf), allocaBytesAligned, minusPtr, plusPtr)
import Foreign.C
  ( CInt (..),
    CSize (..),
    Errno (..),
    eACCES,
    eAGAIN,
    eEXIST,
    eFAULT,
    eINVAL,
    eMFILE,
    eNAMETOOLONG,
    eNFILE,
    eNOENT,
    eNOMEM,
    eNOSPC,
    eNOTDIR,
    eWOULDBLOCK,
    getErrno,
  )
import Foreign.C.ConstPtr (ConstPtr (ConstPtr))
import GHC.TypeLits (Symbol)
import Posix.Inotify.Bindings
import System.IO qualified as IO
import System.OsString.Internal.Types (PosixString (..))
import System.OsString.Posix qualified as OsString
import System.Posix.Types (CSsize (..), Fd (..))
import Prelude hiding (read)

data Event
  = Event
  { wd :: {-# UNPACK #-} !WatchDescriptor,
    mask :: {-# UNPACK #-} !Mask,
    cookie :: {-# UNPACK #-} !Word32,
    name :: !PosixString
  }
  deriving stock (Show)

showEvent :: Event -> [Char]
showEvent event =
  go ""
  where
    go =
      case event.mask of
        QOverflow -> id
        _ -> (show event.wd ++) . (' ' :)
        . case event.mask of
          Access -> ("Access" ++)
          Attrib -> ("Attrib" ++)
          CloseNowrite -> ("CloseNowrite" ++)
          CloseWrite -> ("CloseWrite" ++)
          Create -> ("Create" ++)
          Delete -> ("Delete" ++)
          DeleteSelf -> ("DeleteSelf" ++)
          Modify -> ("Modify" ++)
          MoveSelf -> ("MoveSelf" ++)
          MovedFrom -> ("MovedFrom" ++)
          MovedTo -> ("MovedTo" ++)
          Open -> ("Open" ++)
          Ignored -> ("Ignored" ++)
          QOverflow -> ("QOverflow" ++)
          Unmount -> ("Unmount" ++)
        . case event.mask of
          Ignored -> id
          QOverflow -> id
          Unmount -> id
          _ ->
            (' ' :) . case OsString.decodeWith IO.utf8 event.name of
              Left _ -> ("???" ++)
              Right s -> (show s ++)
        . case event.mask of
          MovedFrom -> ((" cookie=" <> show event.cookie) ++)
          MovedTo -> ((" cookie=" <> show event.cookie) ++)
          _ -> id
        . (if isdir event.mask then (" isdir" ++) else id)

type WatchDescriptor =
  Int

data Inotify
  = Inotify
  { fd :: {-# UNPACK #-} !CInt,
    buffer :: {-# UNPACK #-} !(Ptr ()),
    offsetRef :: {-# UNPACK #-} !(IORef Int),
    buflenRef :: {-# UNPACK #-} !(IORef Int)
  }

newtype Mask
  = Mask Word32
  deriving newtype (Show)

{-# COMPLETE
  Access,
  Attrib,
  CloseNowrite,
  CloseWrite,
  Create,
  Delete,
  DeleteSelf,
  Modify,
  MoveSelf,
  MovedFrom,
  MovedTo,
  Open,
  Ignored,
  QOverflow,
  Unmount
  #-}

newtype Option
  = Option Word32

-- | Perform an action with a new inotify instance.
with :: (Inotify -> IO a) -> IO (Either (Error "inotify_init1") a)
with action =
  Exception.mask \unmask -> do
    -- Initialize an inotify instance with IN_CLOEXEC (to prevent leaking file descriptors on fork, even though no one
    -- really forks in Haskell) and IN_NONBLOCK (because we want to perform nonblocking reads).
    inotify_init1 (_IN_CLOEXEC .|. _IN_NONBLOCK) >>= \case
      -1 -> do
        errno@(Errno n) <- getErrno
        pure $
          Left case () of
            _ | errno == eMFILE -> EMFILE
            _ | errno == eNFILE -> ENFILE
            _ | errno == eNOMEM -> ENOMEM
            _ | otherwise -> error ("inotify_init1: unexpected errno " ++ show n)
      fd -> do
        (`Exception.finally` c_close fd) do
          unmask do
            offsetRef <- newIORef 0
            buflenRef <- newIORef 0
            allocaBytesAligned 4096 (sizeOf (0 :: Int)) \buffer ->
              Right <$> action Inotify {fd, buffer, offsetRef, buflenRef}

-- | Add a new watch, or modify an existing watch.
watch :: Inotify -> PosixString -> [Mask] -> [Option] -> IO (Either (Error "inotify_add_watch") WatchDescriptor)
watch inotify (PosixString path) mask opts =
  ByteString.Short.useAsCString path \cpath ->
    inotify_add_watch inotify.fd (ConstPtr cpath) (combine mask opts) >>= \case
      -1 -> do
        errno@(Errno n) <- getErrno
        pure $
          -- omit EBADF because we expect our inotify fd
          Left case () of
            _ | errno == eACCES -> EACCES
            _ | errno == eEXIST -> EEXIST
            _ | errno == eFAULT -> EFAULT
            _ | errno == eINVAL -> EINVAL
            _ | errno == eNAMETOOLONG -> ENAMETOOLONG
            _ | errno == eNOENT -> ENOENT
            _ | errno == eNOMEM -> ENOMEM
            _ | errno == eNOSPC -> ENOSPC
            _ | errno == eNOTDIR -> ENOTDIR
            _ | otherwise -> error ("inotify_add_watch: unexpected errno " ++ show n)
      wd -> pure (Right (fromIntegral @CInt @Int wd))

combine :: [Mask] -> [Option] -> Word32
combine masks options =
  List.foldl'
    (.|.)
    (List.foldl' (.|.) 0 (coerce @[Mask] @[Word32] masks))
    (coerce @[Option] @[Word32] options)

-- | Remove a watch.
unwatch :: Inotify -> WatchDescriptor -> IO ()
unwatch inotify wd =
  -- ignore errors here because they aren't interesting -
  --   EBADF, we expect our inotify fd so we won't get this
  --   EINVAL, ditto, and maybe this wd was already unwatched; returning unit seems fine
  void (inotify_rm_watch inotify.fd (fromIntegral @Int @CInt wd))

-- | Await an event.
await :: Inotify -> IO Event
await inotify = do
  offset <- readIORef inotify.offsetRef
  buflen <- readIORef inotify.buflenRef
  offset1 <-
    if offset >= buflen
      then do
        len <- readloop inotify.fd inotify.buffer 4096
        writeIORef inotify.offsetRef 0
        writeIORef inotify.buflenRef (fromIntegral @CSsize @Int len)
        pure 0
      else pure offset
  parseEvent inotify offset1

readloop :: CInt -> Ptr a -> CSize -> IO CSsize
readloop fd buf size =
  c_read fd buf size >>= \case
    -1 -> do
      errno@(Errno n) <- getErrno
      if errno == eAGAIN || errno == eWOULDBLOCK
        then do
          threadWaitRead (Fd fd)
          readloop fd buf size
        else error ("read: unexpected errno " ++ show n)
    len -> pure len

-- | Poll for an event.
poll :: Inotify -> IO (Maybe Event)
poll inotify = do
  offset <- readIORef inotify.offsetRef
  buflen <- readIORef inotify.buflenRef
  if offset >= buflen
    then do
      c_read inotify.fd inotify.buffer 4096 >>= \case
        -1 -> do
          errno@(Errno n) <- getErrno
          if errno == eAGAIN || errno == eWOULDBLOCK
            then pure Nothing
            else error ("read: unexpected errno " ++ show n)
        len -> do
          writeIORef inotify.offsetRef 0
          writeIORef inotify.buflenRef (fromIntegral @CSsize @Int len)
          Just <$> parseEvent inotify 0
    else Just <$> parseEvent inotify offset

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
        end <- c_rawmemchr (ConstPtr beginning) 0
        ByteString.Short.createFromPtr beginning (minusPtr end beginning)
  writeIORef inotify.offsetRef $! offset + sizeOfCInt + 12 + fromIntegral @Word32 @Int len
  pure
    Event
      { wd = fromIntegral @CInt @Int wd,
        mask = Mask mask,
        cookie,
        name = PosixString name
      }

has :: Word32 -> Mask -> Bool
has bit (Mask set) =
  set .&. bit /= 0

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

-- | A watch was removed.
pattern Ignored :: Mask
pattern Ignored <- (has _IN_IGNORED -> True)

-- | The event queue overflowed.
pattern QOverflow :: Mask
pattern QOverflow <- (has _IN_Q_OVERFLOW -> True)

-- | The filesystem containing a watched object was unmounted.
pattern Unmount :: Mask
pattern Unmount <- (has _IN_UNMOUNT -> True)

-- | The subject of this event is a directory.
isdir :: Mask -> Bool
isdir =
  has _IN_ISDIR

-- | Equivalent to 'Access', 'Attrib', 'CloseNowrite', 'CloseWrite', 'Create', 'Delete', 'DeleteSelf', 'Modify', 'MoveSelf', 'MovedFrom', 'MovedTo', and 'Open' together.
allEvents :: Mask
allEvents = Mask _IN_ALL_EVENTS

-- | Equivalent to 'CloseWrite' and 'CloseNowrite' together.
close :: Mask
close = Mask _IN_CLOSE

-- | Equivalent to 'MovedFrom' and 'MovedTo' together.
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

------------------------------------------------------------------------------------------------------------------------
-- Errors

type family CanReturnEACCES (s :: Symbol) :: Bool where
  CanReturnEACCES "inotify_add_watch" = 'True
  CanReturnEACCES _ = 'False

type family CanReturnEEXIST (s :: Symbol) :: Bool where
  CanReturnEEXIST "inotify_add_watch" = 'True
  CanReturnEEXIST _ = 'False

type family CanReturnEFAULT (s :: Symbol) :: Bool where
  CanReturnEFAULT "inotify_add_watch" = 'True
  CanReturnEFAULT _ = 'False

type family CanReturnEINVAL (s :: Symbol) :: Bool where
  CanReturnEINVAL "inotify_add_watch" = 'True
  CanReturnEINVAL _ = 'False

type family CanReturnEMFILE (s :: Symbol) :: Bool where
  CanReturnEMFILE "inotify_init1" = 'True
  CanReturnEMFILE _ = 'False

type family CanReturnENAMETOOLONG (s :: Symbol) :: Bool where
  CanReturnENAMETOOLONG "inotify_add_watch" = 'True
  CanReturnENAMETOOLONG _ = 'False

type family CanReturnENFILE (s :: Symbol) :: Bool where
  CanReturnENFILE "inotify_init1" = 'True
  CanReturnENFILE _ = 'False

type family CanReturnENOENT (s :: Symbol) :: Bool where
  CanReturnENOENT "inotify_add_watch" = 'True
  CanReturnENOENT _ = 'False

type family CanReturnENOMEM (s :: Symbol) :: Bool where
  CanReturnENOMEM "inotify_add_watch" = 'True
  CanReturnENOMEM "inotify_init1" = 'True
  CanReturnENOMEM _ = 'False

type family CanReturnENOSPC (s :: Symbol) :: Bool where
  CanReturnENOSPC "inotify_add_watch" = 'True
  CanReturnENOSPC _ = 'False

type family CanReturnENOTDIR (s :: Symbol) :: Bool where
  CanReturnENOTDIR "inotify_add_watch" = 'True
  CanReturnENOTDIR _ = 'False

data Error (s :: Symbol) where
  EACCES :: (CanReturnEACCES s ~ 'True) => Error s
  EEXIST :: (CanReturnEEXIST s ~ 'True) => Error s
  EFAULT :: (CanReturnEFAULT s ~ 'True) => Error s
  EINVAL :: (CanReturnEINVAL s ~ 'True) => Error s
  EMFILE :: (CanReturnEMFILE s ~ 'True) => Error s
  ENAMETOOLONG :: (CanReturnENAMETOOLONG s ~ 'True) => Error s
  ENFILE :: (CanReturnENFILE s ~ 'True) => Error s
  ENOENT :: (CanReturnENOENT s ~ 'True) => Error s
  ENOMEM :: (CanReturnENOMEM s ~ 'True) => Error s
  ENOSPC :: (CanReturnENOSPC s ~ 'True) => Error s
  ENOTDIR :: (CanReturnENOTDIR s ~ 'True) => Error s

instance Show (Error s) where
  show = \case
    EACCES -> "EACCES"
    EEXIST -> "EEXIST"
    EFAULT -> "EFAULT"
    EINVAL -> "EINVAL"
    EMFILE -> "EMFILE"
    ENAMETOOLONG -> "ENAMETOOLONG"
    ENFILE -> "ENFILE"
    ENOENT -> "ENOENT"
    ENOMEM -> "ENOMEM"
    ENOSPC -> "ENOSPC"
    ENOTDIR -> "ENOTDIR"

------------------------------------------------------------------------------------------------------------------------
-- Foreign calls

foreign import capi unsafe "unistd.h close"
  c_close :: CInt -> IO CInt

foreign import capi unsafe "string.h rawmemchr"
  c_rawmemchr :: ConstPtr a -> CInt -> IO (Ptr b)

foreign import capi unsafe "unistd.h read"
  c_read :: CInt -> Ptr a -> CSize -> IO CSsize
