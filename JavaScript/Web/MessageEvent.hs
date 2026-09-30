{-# LANGUAGE CPP #-}
{-# LANGUAGE ForeignFunctionInterface, JavaScriptFFI, DeriveDataTypeable,
             UnboxedTuples, GHCForeignImportPrim, UnliftedFFITypes,
             MagicHash
  #-}

module JavaScript.Web.MessageEvent ( MessageEvent
                                   , getData
                                   , MessageEventData(..)
                                   ) where

import GHCJS.Types

import GHC.Exts

import Data.Typeable

import JavaScript.Web.MessageEvent.Internal

import JavaScript.Web.Blob.Internal (Blob, SomeBlob(..))
import JavaScript.TypedArray.ArrayBuffer.Internal (ArrayBuffer, SomeArrayBuffer(..))
import Data.JSString.Internal.Type (JSString(..))


data MessageEventData = StringData      JSString
                      | BlobData        Blob
                      | ArrayBufferData ArrayBuffer
  deriving (Typeable)

getData :: MessageEvent -> MessageEventData
#if defined(wasm32_HOST_ARCH)
getData me = case js_getDataType me of
               1 -> StringData      (JSString r)
               2 -> BlobData        (SomeBlob r)
               3 -> ArrayBufferData (SomeArrayBuffer r)
  where r = js_getDataVal me
#else
getData me = case js_getData me of
               (# 1#, r #) -> StringData      (JSString r)
               (# 2#, r #) -> BlobData        (SomeBlob r)
               (# 3#, r #) -> ArrayBufferData (SomeArrayBuffer r)
#endif
{-# INLINE getData #-}



-- -----------------------------------------------------------------------------

#if defined(wasm32_HOST_ARCH)
foreign import javascript unsafe
  "var r2 = $1.data; return typeof r2 === 'string' ? 1 : (r2 instanceof ArrayBuffer ? 3 : 2);"
  js_getDataType :: MessageEvent -> Int
foreign import javascript unsafe
  "$1.data"
  js_getDataVal :: MessageEvent -> JSVal
#else
-- (This string used string gaps before the module needed CPP, which
-- mangles them; it is the same JavaScript code on a single line.)
foreign import javascript unsafe
  "((x) => { var r2 = x.data; var r1 = typeof r2 === 'string' ? 1 : (r2 instanceof ArrayBuffer ? 3 : 2); h$ret1 = r2; return r1; })"
  js_getData :: MessageEvent -> (# Int#, JSVal #)
#endif
