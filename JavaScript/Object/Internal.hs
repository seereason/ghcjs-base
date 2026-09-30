{-# LANGUAGE CPP #-}
{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE ForeignFunctionInterface #-}
{-# LANGUAGE JavaScriptFFI #-}
{-# LANGUAGE UnboxedTuples #-}
{-# LANGUAGE GHCForeignImportPrim #-}
{-# LANGUAGE EmptyDataDecls #-}
{-# LANGUAGE UnliftedFFITypes #-}

module JavaScript.Object.Internal
    ( Object(..)
    , create
    , allProps
    , listProps
    , getProp
    , unsafeGetProp
    , setProp
    , unsafeSetProp
    , isInstanceOf
    ) where

import           Data.JSString
import           Data.Typeable

import qualified GHC.JS.Prim                as Prim
import           GHCJS.Types

import qualified JavaScript.Array          as JA
import           JavaScript.Array.Internal (JSArray, SomeJSArray(..))

import           Unsafe.Coerce
import qualified GHC.Exts as Exts
#if defined(wasm32_HOST_ARCH)
import           Control.Exception (throwIO)
import           Data.JSString.Internal.Type (JSString(..))
#endif

newtype Object = Object JSVal deriving (Typeable)
instance IsJSVal Object

-- | create an empty object
create :: IO Object
create = js_create
{-# INLINE create #-}

allProps :: Object -> IO JSArray
allProps o = js_allProps o
{-# INLINE allProps #-}

#if defined(wasm32_HOST_ARCH)
-- h$listProps conses the properties onto a list in for-in order, so the
-- result is in reverse for-in order
listProps :: Object -> IO [JSString]
listProps o = do
  SomeJSArray a <- js_allProps o
  ps <- Prim.fromJSArray a
  return (Prelude.reverse (Prelude.map JSString ps))
{-# INLINE listProps #-}
#else
listProps :: Object -> IO [JSString]
listProps o = unsafeCoerce (js_listProps o)
{-# INLINE listProps #-}
#endif

{- | get a property from an object. If accessing the property results in
     an exception, the exception is converted to a JSException. Since exception
     handling code prevents some optimizations in some JS engines, you may want
     to use unsafeGetProp instead
 -}
getProp :: JSString -> Object -> IO JSVal
getProp p o = js_getProp p o
{-# INLINE getProp #-}

unsafeGetProp :: JSString -> Object -> IO JSVal
unsafeGetProp p o = js_unsafeGetProp p o
{-# INLINE unsafeGetProp #-}

setProp :: JSString -> JSVal -> Object -> IO ()
setProp p v o = js_setProp p v o
{-# INLINE setProp #-}

unsafeSetProp :: JSString -> JSVal -> Object -> IO ()
unsafeSetProp p v o = js_unsafeSetProp p v o
{-# INLINE unsafeSetProp #-}

isInstanceOf :: Object -> JSVal -> Bool
isInstanceOf o s = js_isInstanceOf o s
{-# INLINE isInstanceOf #-}

-- -----------------------------------------------------------------------------

#if defined(wasm32_HOST_ARCH)
foreign import javascript unsafe "(() => { return {}; })()"
#else
foreign import javascript unsafe "(() => { return {}; })"
#endif
  js_create        :: IO Object
#if defined(wasm32_HOST_ARCH)
-- On the JavaScript backend the 'safe' import turns a JavaScript
-- exception into a 'JSException'.  A synchronous wasm import cannot do
-- that, so the exception is caught in JavaScript, returned wrapped in a
-- marker object and rethrown from Haskell.
foreign import javascript unsafe
  "try { return $2[$1]; } catch(e) { return { [Symbol.for('ghcjs-base.exception')]: e }; }"
  js_getProp_wasm  :: JSString -> Object -> IO JSVal
js_getProp       :: JSString -> Object -> IO JSVal
js_getProp p o = js_getProp_wasm p o >>= rethrowJSException
{-# INLINE js_getProp #-}
#else
foreign import javascript safe   "((x,y) => { return y[x]; })"
  js_getProp       :: JSString -> Object -> IO JSVal
#endif
#if defined(wasm32_HOST_ARCH)
foreign import javascript unsafe "((x,y) => { return y[x]; })($1,$2)"
#else
foreign import javascript unsafe "((x,y) => { return y[x]; })"
#endif
  js_unsafeGetProp :: JSString -> Object -> IO JSVal
#if defined(wasm32_HOST_ARCH)
-- see js_getProp
foreign import javascript unsafe
  "try { $3[$1] = $2; return undefined; } catch(e) { return { [Symbol.for('ghcjs-base.exception')]: e }; }"
  js_setProp_wasm  :: JSString -> JSVal -> Object -> IO JSVal
js_setProp       :: JSString -> JSVal -> Object -> IO ()
js_setProp p v o = js_setProp_wasm p v o >>= rethrowJSException >> return ()
{-# INLINE js_setProp #-}
#else
foreign import javascript safe   "((x,y,z) => { z[x] = y; })"
  js_setProp       :: JSString -> JSVal -> Object -> IO ()
#endif
#if defined(wasm32_HOST_ARCH)
foreign import javascript unsafe "((x,y,z) => { z[x] = y; })($1,$2,$3)"
#else
foreign import javascript unsafe "((x,y,z) => { z[x] = y; })"
#endif
  js_unsafeSetProp :: JSString -> JSVal -> Object -> IO ()
#if defined(wasm32_HOST_ARCH)
foreign import javascript unsafe "((x,y) => { return x instanceof y; })($1,$2)"
#else
foreign import javascript unsafe "((x,y) => { return x instanceof y; })"
#endif
  js_isInstanceOf  :: Object -> JSVal -> Bool
#if defined(wasm32_HOST_ARCH)
-- h$allProps (jsbits/utils.js)
foreign import javascript unsafe
  "var a = [], i = 0; for(var p in $1) a[i++] = p; return a;"
  js_allProps      :: Object -> IO JSArray

-- If the value is a marker object made by a snippet's catch clause, throw
-- the wrapped JavaScript exception as a 'JSException'.
rethrowJSException :: JSVal -> IO JSVal
rethrowJSException r
  | js_isExceptionMarker r = throwIO (JSException (js_exceptionMarkerValue r))
  | otherwise              = return r
{-# INLINE rethrowJSException #-}

foreign import javascript unsafe
  "typeof $1 === 'object' && $1 !== null && Object.prototype.hasOwnProperty.call($1, Symbol.for('ghcjs-base.exception'))"
  js_isExceptionMarker :: JSVal -> Bool
foreign import javascript unsafe
  "$1[Symbol.for('ghcjs-base.exception')]"
  js_exceptionMarkerValue :: JSVal -> JSVal
#else
foreign import javascript unsafe  "h$allProps"
  js_allProps      :: Object -> IO JSArray
foreign import javascript unsafe  "h$listProps"
  js_listProps     :: Object -> IO Exts.Any -- [JSString]
#endif
