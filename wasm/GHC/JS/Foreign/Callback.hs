{- | The GHC wasm backend's stand-in for the JavaScript backend's
     @GHC.JS.Foreign.Callback@ (which base only exposes on
     @arch(javascript)@).  Built only for @arch(wasm32)@.

     Callbacks are JavaScript functions created with the wasm JSFFI's
     @foreign import javascript "wrapper"@ (asynchronous: calling them
     returns a Promise and the Haskell code runs as its own thread) and
     @"wrapper sync"@ (synchronous: the Haskell code runs to completion
     before the JavaScript call returns, so event handlers can call
     @preventDefault@ etc.).

     'OnBlocked' is accepted for API compatibility.  A synchronous
     callback whose Haskell code blocks is not resumed asynchronously as
     on the JavaScript backend.
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
syncCallback _ x = Callback <$> js_sync0 x

syncCallback1 :: OnBlocked -> (JSVal -> IO ()) -> IO (Callback (JSVal -> IO ()))
syncCallback1 _ x = Callback <$> js_sync1 x

syncCallback2 :: OnBlocked -> (JSVal -> JSVal -> IO ()) -> IO (Callback (JSVal -> JSVal -> IO ()))
syncCallback2 _ x = Callback <$> js_sync2 x

syncCallback3 :: OnBlocked -> (JSVal -> JSVal -> JSVal -> IO ()) -> IO (Callback (JSVal -> JSVal -> JSVal -> IO ()))
syncCallback3 _ x = Callback <$> js_sync3 x

syncCallback' :: IO JSVal -> IO (Callback (IO JSVal))
syncCallback' x = Callback <$> js_syncR0 x

syncCallback1' :: (JSVal -> IO JSVal) -> IO (Callback (JSVal -> IO JSVal))
syncCallback1' x = Callback <$> js_syncR1 x

syncCallback2' :: (JSVal -> JSVal -> IO JSVal) -> IO (Callback (JSVal -> JSVal -> IO JSVal))
syncCallback2' x = Callback <$> js_syncR2 x

syncCallback3' :: (JSVal -> JSVal -> JSVal -> IO JSVal) -> IO (Callback (JSVal -> JSVal -> JSVal -> IO JSVal))
syncCallback3' x = Callback <$> js_syncR3 x

asyncCallback :: IO () -> IO (Callback (IO ()))
asyncCallback x = Callback <$> js_async0 x

asyncCallback1 :: (JSVal -> IO ()) -> IO (Callback (JSVal -> IO ()))
asyncCallback1 x = Callback <$> js_async1 x

asyncCallback2 :: (JSVal -> JSVal -> IO ()) -> IO (Callback (JSVal -> JSVal -> IO ()))
asyncCallback2 x = Callback <$> js_async2 x

asyncCallback3 :: (JSVal -> JSVal -> JSVal -> IO ()) -> IO (Callback (JSVal -> JSVal -> JSVal -> IO ()))
asyncCallback3 x = Callback <$> js_async3 x

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
