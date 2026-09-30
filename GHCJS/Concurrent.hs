{-# LANGUAGE CPP #-}
{-# LANGUAGE ForeignFunctionInterface, JavaScriptFFI,
             UnliftedFFITypes, DeriveDataTypeable, MagicHash
  #-}

{- | GHCJS has two types of threads. Regular, asynchronous threads are
     started with `h$run`, are managed by the scheduler and run in the
     background. `h$run` returns immediately.

     Synchronous threads are started with `h$runSync`, which returns
     when the thread has run to completion. When a synchronous thread
     does an operation that would block, like accessing an MVar or
     an asynchronous FFI call, it cannot continue synchronously.

     There are two ways this can be resolved, depending on the
     second argument of the `h$runSync` call:

      * The action is aborted and the thread receives a 'WouldBlockException'
      * The thread continues asynchronously, `h$runSync` returns

     Note: when a synchronous thread encounters a black hole from
     another thread, it tries to steal the work from that thread
     to avoid blocking. In some cases that might not be possible,
     for example when the data accessed is produced by a lazy IO
     operation. This is resolved the same way as blocking on an IO
     action would be.
 -}

module GHCJS.Concurrent ( isThreadSynchronous
                        , isThreadContinueAsync
                        , OnBlocked(..)
                        , WouldBlockException(..)
                        , withoutPreemption
                        , synchronously
                        ) where

import           GHC.JS.Prim
import           GHC.JS.Foreign.Callback (OnBlocked(..))

import           Control.Applicative
import           Control.Concurrent
import qualified Control.Exception as Ex

import           GHC.Exts (ThreadId#)
import           GHC.Conc.Sync (ThreadId(..))

import           Data.Bits (testBit)
import           Data.Data
import           Data.Typeable

import           Unsafe.Coerce
#if defined(wasm32_HOST_ARCH)
import           Data.Bits (setBit, clearBit, (.|.), (.&.))
import           Data.IORef
import qualified Data.Map.Strict as M
import           System.IO.Unsafe (unsafePerformIO)
#endif

{- |
     Run the action without the scheduler preempting the thread. When a blocking
     action is encountered, the thread is still suspended and will continue
     without preemption when it's woken up again.

     When the thread encounters a black hole from another thread, the scheduler
     will attempt to clear it by temporarily switching to that thread.
 -}

withoutPreemption :: IO a -> IO a
withoutPreemption x = Ex.mask $ \restore -> do
  oldS <- js_setNoPreemption True
  if oldS
    then restore x
    else restore x `Ex.finally` js_setNoPreemption False
{-# INLINE withoutPreemption #-}


{- |
     Run the action synchronously, which means that the thread will not
     be preempted by the scheduler. If the thread encounters a blocking
     operation, the runtime throws a WouldBlock exception.

     When the thread encounters a black hole from another thread, the scheduler
     will attempt to clear it by temporarily switching to that thread.
 -}
synchronously :: IO a -> IO a
synchronously x = Ex.mask $ \restore -> do
  oldS <- js_setSynchronous True
  if oldS
    then restore x
    else restore x `Ex.finally` js_setSynchronous False
{-# INLINE synchronously #-}

{- | Returns whether the 'ThreadId' is a synchronous thread
 -}
isThreadSynchronous :: ThreadId -> IO Bool
isThreadSynchronous = fmap (`testBit` 0) . syncThreadState

{- |
     Returns whether the 'ThreadId' will continue running async. Always
     returns 'True' when the thread is not synchronous.
 -}
isThreadContinueAsync :: ThreadId -> IO Bool
isThreadContinueAsync = fmap (`testBit` 1) . syncThreadState

{- |
     Returns whether the 'ThreadId' is not preemptible. Always
     returns 'True' when the thread is synchronous.
 -}
isThreadNonPreemptible :: ThreadId -> IO Bool
isThreadNonPreemptible = fmap (`testBit` 2) . syncThreadState

syncThreadState :: ThreadId-> IO Int
syncThreadState (ThreadId tid) = js_syncThreadState tid

-- ----------------------------------------------------------------------------

#if defined(wasm32_HOST_ARCH)
{- The wasm RTS has no synchronous threads (h$runSync) and no
   noPreemption / isSynchronous thread flags.  To keep the API, the flags
   set by 'withoutPreemption' and 'synchronously' are recorded per thread
   here and reported by 'syncThreadState', but the scheduler does not act
   on them (in particular 'synchronously' does not make blocking
   operations throw 'WouldBlockException').

   Flag bits as returned by h$syncThreadState:
     bit 0: synchronous, bit 1: continue async, bit 2: non-preemptible
 -}
threadFlags :: IORef (M.Map ThreadId Int)
threadFlags = unsafePerformIO (newIORef M.empty)
{-# NOINLINE threadFlags #-}

-- set or clear a flag bit for the current thread, returning the old value
setThreadFlag :: Int -> Bool -> IO Bool
setThreadFlag bit x = do
  t <- myThreadId
  atomicModifyIORef' threadFlags $ \m ->
    let old  = M.findWithDefault 0 t m
        new  = if x then setBit old bit else clearBit old bit
        m'   = if new == 0 then M.delete t m else M.insert t new m
    in  (m', testBit old bit)

js_syncThreadState :: ThreadId# -> IO Int
js_syncThreadState tid = do
  m <- readIORef threadFlags
  let f = M.findWithDefault 0 (ThreadId tid) m
  return $ if testBit f 0
             then 1 .|. 4           -- synchronous: not continue-async, non-preemptible
             else 2 .|. (f .&. 4)   -- asynchronous: continue-async

js_setNoPreemption :: Bool -> IO Bool
js_setNoPreemption = setThreadFlag 2

js_setSynchronous :: Bool -> IO Bool
js_setSynchronous = setThreadFlag 0
#else
foreign import javascript unsafe "h$syncThreadState"
  js_syncThreadState :: ThreadId# -> IO Int

foreign import javascript unsafe
  "((x) => { var r = h$currentThread.noPreemption; h$currentThread.noPreemption = x; return r; })"
  js_setNoPreemption :: Bool -> IO Bool;

foreign import javascript unsafe
  "((x) => { var r = h$currentThread.isSynchronous; h$currentThread.isSynchronous = x; return r; })"
  js_setSynchronous :: Bool -> IO Bool
#endif
