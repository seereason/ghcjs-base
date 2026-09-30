{-# LANGUAGE CPP #-}
{-# LANGUAGE ForeignFunctionInterface, JavaScriptFFI #-}

module JavaScript.Web.Worker ( Worker
                             , create
                             , postMessage
                             , terminate
                             ) where

import GHC.JS.Prim

import Data.JSString
import Data.Typeable
#if defined(wasm32_HOST_ARCH)
-- the wasm JSFFI only unwraps newtypes whose constructors are in scope
import Data.JSString.Internal.Type (JSString(..))
#endif

newtype Worker = Worker JSVal deriving Typeable

create :: JSString -> IO Worker
create script = js_create script
{-# INLINE create #-}

postMessage :: JSVal -> Worker -> IO ()
postMessage msg w = js_postMessage msg w
{-# INLINE postMessage #-}

terminate :: Worker -> IO ()
terminate w = js_terminate w
{-# INLINE terminate #-}

-- -----------------------------------------------------------------------------

#if defined(wasm32_HOST_ARCH)
foreign import javascript unsafe "(($1) => { return new Worker($1); })($1)" js_create :: JSString -> IO Worker
#else
foreign import javascript unsafe 
  "(($1) => { return new Worker($1); })" js_create :: JSString -> IO Worker
#endif
#if defined(wasm32_HOST_ARCH)
foreign import javascript unsafe "((x,y) => { y.postMessage(x); })($1,$2)" js_postMessage  :: JSVal -> Worker -> IO ()
#else
foreign import javascript unsafe
  "((x,y) => { y.postMessage(x); })" js_postMessage  :: JSVal -> Worker -> IO ()
#endif
#if defined(wasm32_HOST_ARCH)
foreign import javascript unsafe "((x) => { x.terminate(); })($1)" js_terminate :: Worker -> IO ()
#else
foreign import javascript unsafe
  "((x) => { x.terminate(); })" js_terminate :: Worker -> IO ()
#endif
