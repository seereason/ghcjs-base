{-# LANGUAGE CPP #-}
{-# LANGUAGE ForeignFunctionInterface, JavaScriptFFI #-}

module JavaScript.Web.File ( File
                             -- Blob operations
                           , size
                           , contentType
                           , slice
                           , isClosed
                           , close
                             -- additional File operations
                           , name
                           , lastModified
                           ) where

import JavaScript.Web.Blob.Internal

import Data.JSString
#if defined(wasm32_HOST_ARCH)
-- the wasm JSFFI only unwraps newtypes whose constructors are in scope
import Data.JSString.Internal.Type (JSString(..))
#endif

name :: File -> JSString
name b = js_name b
{-# INLINE name #-}

lastModified :: File -> Double
lastModified b = js_lastModified b
{-# INLINE lastModified #-}

-- -----------------------------------------------------------------------------

#if defined(wasm32_HOST_ARCH)
foreign import javascript unsafe "((x) => { return x.name; })($1)"         js_name         :: File -> JSString
#else
foreign import javascript unsafe "((x) => { return x.name; })"         js_name         :: File -> JSString
#endif
#if defined(wasm32_HOST_ARCH)
foreign import javascript unsafe "((x) => { return x.lastModified; })($1)" js_lastModified :: File -> Double
#else
foreign import javascript unsafe "((x) => { return x.lastModified; })" js_lastModified :: File -> Double
#endif

