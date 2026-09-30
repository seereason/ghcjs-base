{-# LANGUAGE CPP #-}
{-# LANGUAGE ForeignFunctionInterface #-}
{-# LANGUAGE JavaScriptFFI #-}

module JavaScript.Web.ErrorEvent ( ErrorEvent
                                 , message
                                 , filename
                                 , lineno
                                 , colno
                                 , error
                                 ) where

import Prelude hiding (error)

import GHCJS.Types

import Data.JSString

import JavaScript.Web.ErrorEvent.Internal
#if defined(wasm32_HOST_ARCH)
-- the wasm JSFFI only unwraps newtypes whose constructors are in scope
import Data.JSString.Internal.Type (JSString(..))
#endif

message :: ErrorEvent -> JSString
message ee = js_getMessage ee
{-# INLINE message #-}

filename :: ErrorEvent -> JSString
filename ee = js_getFilename ee
{-# INLINE filename #-}

lineno :: ErrorEvent -> Int
lineno ee = js_getLineno ee
{-# INLINE lineno #-}

colno :: ErrorEvent -> Int
colno ee = js_getColno ee
{-# INLINE colno #-}

error :: ErrorEvent -> JSVal
error ee = js_getError ee
{-# INLINE error #-}

-- -----------------------------------------------------------------------------

#if defined(wasm32_HOST_ARCH)
foreign import javascript unsafe "((x) => { return x.message; })($1)"
#else
foreign import javascript unsafe "((x) => { return x.message; })"
#endif
  js_getMessage  :: ErrorEvent -> JSString
#if defined(wasm32_HOST_ARCH)
foreign import javascript unsafe "((x) => { return x.filename; })($1)"
#else
foreign import javascript unsafe "((x) => { return x.filename; })"
#endif
  js_getFilename :: ErrorEvent -> JSString
#if defined(wasm32_HOST_ARCH)
foreign import javascript unsafe "((x) => { return x.lineno; })($1)"
#else
foreign import javascript unsafe "((x) => { return x.lineno; })"
#endif
  js_getLineno   :: ErrorEvent -> Int
#if defined(wasm32_HOST_ARCH)
foreign import javascript unsafe "((x) => { return x.colno; })($1)"
#else
foreign import javascript unsafe "((x) => { return x.colno; })"
#endif
  js_getColno    :: ErrorEvent -> Int
#if defined(wasm32_HOST_ARCH)
foreign import javascript unsafe "((x) => { return x.error; })($1)"
#else
foreign import javascript unsafe "((x) => { return x.error; })"
#endif
  js_getError    :: ErrorEvent -> JSVal
