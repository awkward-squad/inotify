{-# LANGUAGE CApiFFI #-}

#include <sys/inotify.h>

module Posix.Inotify.Bindings
  ( -- * Functions
    inotify_init,
    inotify_init1,
    inotify_add_watch,
    inotify_rm_watch,

    -- * Initialize flags
    _IN_NONBLOCK,
    _IN_CLOEXEC,

    -- * Events
    _IN_ACCESS,
    _IN_ATTRIB,
    _IN_CLOSE_WRITE,
    _IN_CLOSE_NOWRITE,
    _IN_CREATE,
    _IN_DELETE,
    _IN_DELETE_SELF,
    _IN_MODIFY,
    _IN_MOVE_SELF,
    _IN_MOVED_FROM,
    _IN_MOVED_TO,
    _IN_OPEN,

    -- ** Convenience macros
    _IN_ALL_EVENTS,
    _IN_MOVE,
    _IN_CLOSE,

    -- * Add watch flags
    _IN_DONT_FOLLOW,
    _IN_EXCL_UNLINK,
    _IN_MASK_ADD,
    _IN_ONESHOT,
    _IN_ONLYDIR,
    _IN_MASK_CREATE,

    -- * Read flags
    _IN_IGNORED,
    _IN_ISDIR,
    _IN_Q_OVERFLOW,
    _IN_UNMOUNT,
  )
where

import Data.Word (Word32)
import Foreign.C (CChar (..), CInt (..))
import Foreign.C.ConstPtr (ConstPtr (..))

foreign import capi unsafe "sys/inotify.h inotify_init"
  inotify_init :: IO CInt

foreign import capi unsafe "sys/inotify.h inotify_init1"
  inotify_init1 :: CInt -> IO CInt

foreign import capi unsafe "sys/inotify.h inotify_add_watch"
  inotify_add_watch :: CInt -> ConstPtr CChar -> Word32 -> IO CInt

foreign import capi unsafe "sys/inotify.h inotify_rm_watch"
  inotify_rm_watch :: CInt -> CInt -> IO CInt

_IN_NONBLOCK :: CInt
_IN_NONBLOCK = #{const IN_NONBLOCK}

_IN_CLOEXEC :: CInt
_IN_CLOEXEC = #{const IN_CLOEXEC}

_IN_ACCESS :: Word32
_IN_ACCESS = #{const IN_ACCESS}

_IN_ATTRIB :: Word32
_IN_ATTRIB = #{const IN_ATTRIB}

_IN_CLOSE_WRITE :: Word32
_IN_CLOSE_WRITE = #{const IN_CLOSE_WRITE}

_IN_CLOSE_NOWRITE :: Word32
_IN_CLOSE_NOWRITE = #{const IN_CLOSE_NOWRITE}

_IN_CREATE :: Word32
_IN_CREATE = #{const IN_CREATE}

_IN_DELETE :: Word32
_IN_DELETE = #{const IN_DELETE}

_IN_DELETE_SELF :: Word32
_IN_DELETE_SELF = #{const IN_DELETE_SELF}

_IN_MODIFY :: Word32
_IN_MODIFY = #{const IN_MODIFY}

_IN_MOVE_SELF :: Word32
_IN_MOVE_SELF = #{const IN_MOVE_SELF}

_IN_MOVED_FROM :: Word32
_IN_MOVED_FROM = #{const IN_MOVED_FROM}

_IN_MOVED_TO :: Word32
_IN_MOVED_TO = #{const IN_MOVED_TO}

_IN_OPEN :: Word32
_IN_OPEN = #{const IN_OPEN}

_IN_ALL_EVENTS :: Word32
_IN_ALL_EVENTS = #{const IN_ALL_EVENTS}

_IN_MOVE :: Word32
_IN_MOVE = #{const IN_MOVE}

_IN_CLOSE :: Word32
_IN_CLOSE = #{const IN_CLOSE}

_IN_DONT_FOLLOW :: Word32
_IN_DONT_FOLLOW = #{const IN_DONT_FOLLOW}

_IN_EXCL_UNLINK :: Word32
_IN_EXCL_UNLINK = #{const IN_EXCL_UNLINK}

_IN_MASK_ADD :: Word32
_IN_MASK_ADD = #{const IN_MASK_ADD}

_IN_ONESHOT :: Word32
_IN_ONESHOT = #{const IN_ONESHOT}

_IN_ONLYDIR :: Word32
_IN_ONLYDIR = #{const IN_ONLYDIR}

_IN_MASK_CREATE :: Word32
_IN_MASK_CREATE = #{const IN_MASK_CREATE}

_IN_IGNORED :: Word32
_IN_IGNORED = #{const IN_IGNORED}

_IN_ISDIR :: Word32
_IN_ISDIR = #{const IN_ISDIR}

_IN_Q_OVERFLOW :: Word32
_IN_Q_OVERFLOW = #{const IN_Q_OVERFLOW}

_IN_UNMOUNT :: Word32
_IN_UNMOUNT = #{const IN_UNMOUNT}
