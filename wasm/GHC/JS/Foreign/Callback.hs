{- | The GHC wasm backend's stand-in for the JavaScript backend's
     @GHC.JS.Foreign.Callback@ (which base only exposes on
     @arch(javascript)@).  Built only for @arch(wasm32)@.

     Callbacks are JavaScript functions created with the wasm JSFFI's
     @foreign import javascript "wrapper"@ (asynchronous: calling them
     returns a Promise and the Haskell code runs as its own thread) and
     @"wrapper sync"@ (synchronous: the Haskell code runs to completion
     before the JavaScript call returns, so event handlers can call
     @preventDefault@ etc.).

     'OnBlocked' chooses between the two.  On the JavaScript backend a
     @ContinueAsync@ callback runs synchronously until it blocks and then
     continues asynchronously.  A wasm synchronous export cannot block
     at all (it cannot return to the JavaScript event loop while its
     thread waits on an asynchronous import or an 'MVar'), so
     @ContinueAsync@ callbacks are asynchronous exports here: the runtime
     still starts running the Haskell code inside the call and only
     returns (a Promise) once it blocks, so a handler that calls
     @preventDefault@ before blocking behaves as before.
     @ThrowWouldBlock@ callbacks, and the ones that return a value, are
     synchronous exports.

     A synchronous export only runs its own Haskell thread to
     completion.  Threads it wakes (say, by writing an 'MVar' or 'TChan'
     that a dispatcher thread is reading) are left runnable, and the
     wasm RTS only runs them the next time something enters its
     scheduler loop: a resolved Promise, a timer, an asynchronous
     export.  See Note [Async JSFFI scheduler] in GHC's
     rts/wasm/scheduler.cmm: "it's not safe to forget to run it when
     there's still thread that needs to make progress".  So every
     synchronous callback here is wrapped to queue a run of the
     scheduler loop (@__exports.rts_schedulerLoop@, which is idempotent
     and guards against re-entrance) as a microtask once it returns.
 -}
module GHC.JS.Foreign.Callback
    ( Callback(..)
    , OnBlocked(..)
    , releaseCallback
      -- * asynchronous callbacks
    , asyncCallback
    , asyncCallback1
    , asyncCallback2
    , asyncCallback3
      -- * synchronous callbacks
    , syncCallback
    , syncCallback1
    , syncCallback2
    , syncCallback3
      -- * synchronous callbacks that return a value
    , syncCallback'
    , syncCallback1'
    , syncCallback2'
    , syncCallback3'
    ) where

import GHC.Wasm.Prim (JSVal)

-- | A JavaScript function that calls back into Haskell.  The type
--   parameter records the Haskell type of the wrapped action.
newtype Callback a = Callback JSVal

data OnBlocked = ContinueAsync   -- ^ continue the thread asynchronously if it blocks
               | ThrowWouldBlock -- ^ throw 'WouldBlockException' if the thread blocks
               deriving (Eq)

{- | On the JavaScript backend this frees the Haskell side of the
     callback.  On wasm the runtime frees it (via a JavaScript
     @FinalizationRegistry@) once the JavaScript function is garbage
     collected, and code that removes an event listener after releasing
     its callback still needs the 'JSVal', so this does nothing.
 -}
releaseCallback :: Callback a -> IO ()
releaseCallback _ = pure ()

syncCallback :: OnBlocked -> IO () -> IO (Callback (IO ()))
syncCallback ContinueAsync x = Callback <$> js_async0 x
syncCallback ThrowWouldBlock x = Callback <$> (js_sync0 x >>= js_kickScheduler)

syncCallback1 :: OnBlocked -> (JSVal -> IO ()) -> IO (Callback (JSVal -> IO ()))
syncCallback1 ContinueAsync x = Callback <$> js_async1 x
syncCallback1 ThrowWouldBlock x = Callback <$> (js_sync1 x >>= js_kickScheduler)

syncCallback2 :: OnBlocked -> (JSVal -> JSVal -> IO ()) -> IO (Callback (JSVal -> JSVal -> IO ()))
syncCallback2 ContinueAsync x = Callback <$> js_async2 x
syncCallback2 ThrowWouldBlock x = Callback <$> (js_sync2 x >>= js_kickScheduler)

syncCallback3 :: OnBlocked -> (JSVal -> JSVal -> JSVal -> IO ()) -> IO (Callback (JSVal -> JSVal -> JSVal -> IO ()))
syncCallback3 ContinueAsync x = Callback <$> js_async3 x
syncCallback3 ThrowWouldBlock x = Callback <$> (js_sync3 x >>= js_kickScheduler)

syncCallback' :: IO JSVal -> IO (Callback (IO JSVal))
syncCallback' x = Callback <$> (js_syncR0 x >>= js_kickScheduler)

syncCallback1' :: (JSVal -> IO JSVal) -> IO (Callback (JSVal -> IO JSVal))
syncCallback1' x = Callback <$> (js_syncR1 x >>= js_kickScheduler)

syncCallback2' :: (JSVal -> JSVal -> IO JSVal) -> IO (Callback (JSVal -> JSVal -> IO JSVal))
syncCallback2' x = Callback <$> (js_syncR2 x >>= js_kickScheduler)

syncCallback3' :: (JSVal -> JSVal -> JSVal -> IO JSVal) -> IO (Callback (JSVal -> JSVal -> JSVal -> IO JSVal))
syncCallback3' x = Callback <$> (js_syncR3 x >>= js_kickScheduler)

asyncCallback :: IO () -> IO (Callback (IO ()))
asyncCallback x = Callback <$> js_async0 x

asyncCallback1 :: (JSVal -> IO ()) -> IO (Callback (JSVal -> IO ()))
asyncCallback1 x = Callback <$> js_async1 x

asyncCallback2 :: (JSVal -> JSVal -> IO ()) -> IO (Callback (JSVal -> JSVal -> IO ()))
asyncCallback2 x = Callback <$> js_async2 x

asyncCallback3 :: (JSVal -> JSVal -> JSVal -> IO ()) -> IO (Callback (JSVal -> JSVal -> JSVal -> IO ()))
asyncCallback3 x = Callback <$> js_async3 x

-- | Wrap a synchronous callback so that, after it returns, the RTS
--   scheduler loop runs (in a microtask, so not re-entrantly) to make
--   progress in any threads the callback woke.
foreign import javascript unsafe
  "const f = $1; return (...args) => { try { return f(...args); } finally { queueMicrotask(() => __exports.rts_schedulerLoop()); } };"
  js_kickScheduler :: JSVal -> IO JSVal

foreign import javascript "wrapper sync" js_sync0  :: IO () -> IO JSVal
foreign import javascript "wrapper sync" js_sync1  :: (JSVal -> IO ()) -> IO JSVal
foreign import javascript "wrapper sync" js_sync2  :: (JSVal -> JSVal -> IO ()) -> IO JSVal
foreign import javascript "wrapper sync" js_sync3  :: (JSVal -> JSVal -> JSVal -> IO ()) -> IO JSVal
foreign import javascript "wrapper sync" js_syncR0 :: IO JSVal -> IO JSVal
foreign import javascript "wrapper sync" js_syncR1 :: (JSVal -> IO JSVal) -> IO JSVal
foreign import javascript "wrapper sync" js_syncR2 :: (JSVal -> JSVal -> IO JSVal) -> IO JSVal
foreign import javascript "wrapper sync" js_syncR3 :: (JSVal -> JSVal -> JSVal -> IO JSVal) -> IO JSVal
foreign import javascript "wrapper"      js_async0 :: IO () -> IO JSVal
foreign import javascript "wrapper"      js_async1 :: (JSVal -> IO ()) -> IO JSVal
foreign import javascript "wrapper"      js_async2 :: (JSVal -> JSVal -> IO ()) -> IO JSVal
foreign import javascript "wrapper"      js_async3 :: (JSVal -> JSVal -> JSVal -> IO ()) -> IO JSVal
