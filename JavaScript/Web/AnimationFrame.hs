{-# LANGUAGE CPP #-}
{-# LANGUAGE ForeignFunctionInterface #-}
{-# LANGUAGE JavaScriptFFI #-}
{-# LANGUAGE InterruptibleFFI #-}
{-# LANGUAGE DeriveDataTypeable #-}

{- |
     Animation frames are the browser's mechanism for smooth animation.
     An animation frame callback is run just before the browser repaints.

     When the content window is inactive, for example when the user is looking
     at another tab, it can take a long time for an animation frame callback
     to happen. Be careful structuring evaluation around this! Typically this
     means carefully forcing the data before the animation frame is requested,
     so the callback can run quickly and predictably.
  -}

module JavaScript.Web.AnimationFrame
    ( waitForAnimationFrame
    , inAnimationFrame
    , cancelAnimationFrame
    , AnimationFrameHandle
    ) where

import GHC.JS.Foreign.Callback
import GHCJS.Marshal.Pure
import GHCJS.Types

import Control.Exception (onException)
#if defined(wasm32_HOST_ARCH)
import Control.Exception (evaluate)
#endif
import Data.Typeable

newtype AnimationFrameHandle = AnimationFrameHandle JSVal
  deriving (Typeable)

{- |
     Wait for an animation frame callback to continue running the current
     thread. Use 'GHCJS.Concurrent.synchronously' if the thread should
     not be preempted. This will return the high-performance clock time
     stamp once an animation frame is reached.
 -}
waitForAnimationFrame :: IO Double
waitForAnimationFrame = do
  h <- js_makeAnimationFrameHandle
  js_waitForAnimationFrame h `onException` js_cancelAnimationFrame h

{- |
     Run the action in an animationframe callback. The action runs in a
     synchronous thread, and is passed the high-performance clock time
     stamp for that frame.
 -}
inAnimationFrame :: OnBlocked       -- ^ what to do when encountering a blocking call
                 -> (Double -> IO ())  -- ^ the action to run
                 -> IO AnimationFrameHandle
inAnimationFrame onBlocked x = do
  cb <- syncCallback1 onBlocked (x . pFromJSVal)
  h  <- js_makeAnimationFrameHandleCallback (jsval cb)
  js_requestAnimationFrame h
  return h

cancelAnimationFrame :: AnimationFrameHandle -> IO ()
cancelAnimationFrame h = js_cancelAnimationFrame h
{-# INLINE cancelAnimationFrame #-}

-- -----------------------------------------------------------------------------

#if defined(wasm32_HOST_ARCH)
foreign import javascript unsafe "(() => { return { handle: null, callback: null }; })()"
#else
foreign import javascript unsafe "(() => { return { handle: null, callback: null }; })"
#endif
  js_makeAnimationFrameHandle :: IO AnimationFrameHandle
#if defined(wasm32_HOST_ARCH)
foreign import javascript unsafe "(($1) => { return { handle: null, callback: $1 }; })($1)"
#else
foreign import javascript unsafe "(($1) => { return { handle: null, callback: $1 }; })"
#endif
  js_makeAnimationFrameHandleCallback :: JSVal -> IO AnimationFrameHandle
#if defined(wasm32_HOST_ARCH)
-- h$animationFrameCancel (releasing the callback is left to the wasm
-- runtime, see GHC.JS.Foreign.Callback.releaseCallback)
foreign import javascript unsafe
  "var h = $1; if(h.handle) cancelAnimationFrame(h.handle); if(h.callback) { h.callback = null; }"
  js_cancelAnimationFrame :: AnimationFrameHandle -> IO ()
-- asynchronous: the Promise resolves with the frame's time stamp
foreign import javascript safe
  "var h = $1; return new Promise((resolve) => { h.handle = requestAnimationFrame(resolve); });"
  js_waitForAnimationFrame_wasm :: AnimationFrameHandle -> IO Double
-- block the calling thread until the frame (as the JavaScript backend's
-- interruptible import does), not just until the result is demanded
js_waitForAnimationFrame :: AnimationFrameHandle -> IO Double
js_waitForAnimationFrame h = js_waitForAnimationFrame_wasm h >>= evaluate
-- h$animationFrameRequest
foreign import javascript unsafe
  "var h = $1; h.handle = requestAnimationFrame((ts) => { var cb = h.callback; if(cb) { h.callback = null; cb(ts); } });"
  js_requestAnimationFrame :: AnimationFrameHandle -> IO ()
#else
foreign import javascript unsafe "h$animationFrameCancel"
  js_cancelAnimationFrame :: AnimationFrameHandle -> IO ()
foreign import javascript interruptible
  "((x,c) => { return x.handle = requestAnimationFrame(c); })"
  js_waitForAnimationFrame :: AnimationFrameHandle -> IO Double
foreign import javascript unsafe "h$animationFrameRequest"
  js_requestAnimationFrame :: AnimationFrameHandle -> IO ()
#endif
