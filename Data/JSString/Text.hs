{-# LANGUAGE ForeignFunctionInterface, UnliftedFFITypes, JavaScriptFFI,
    UnboxedTuples, DeriveDataTypeable, GHCForeignImportPrim,
    MagicHash, FlexibleInstances, BangPatterns, Rank2Types, CPP #-}

{- | Conversion between 'Data.Text.Text' and 'Data.JSString.JSString'

 -}

module Data.JSString.Text
    ( textToJSString
    , textFromJSString
    , lazyTextToJSString
    , lazyTextFromJSString
    , textFromJSVal
    , lazyTextFromJSVal
    ) where

import GHC.JS.Prim

import GHC.Exts (ByteArray#, Int(..), Int#, Any)

import Control.DeepSeq

import qualified Data.Text.Array as A
import qualified Data.Text as T
import qualified Data.Text.Internal as T
import qualified Data.Text.Lazy as TL

import Data.JSString.Internal.Type

import Unsafe.Coerce
#if defined(wasm32_HOST_ARCH)
import Data.Word (Word8)
import Foreign.Ptr (Ptr)
import Foreign.Marshal.Alloc (allocaBytes)
import System.IO.Unsafe (unsafeDupablePerformIO)
import qualified Data.Text.Foreign as TF
#endif

textToJSString :: T.Text -> JSString
textToJSString (T.Text (A.ByteArray ba) (I# offset) (I# length)) =
  js_toString ba offset length
{-# INLINE textToJSString #-}

textFromJSString :: JSString -> T.Text
textFromJSString j =
  case js_fromString j of
    (# _ , 0#     #) -> T.empty
    (# ba, length #) -> T.Text (A.ByteArray ba) 0 (I# length)
{-# INLINE  textFromJSString #-}

lazyTextToJSString :: TL.Text -> JSString
lazyTextToJSString t = rnf t `seq` js_lazyTextToString (unsafeCoerce t)
{-# INLINE lazyTextToJSString #-}

lazyTextFromJSString :: JSString -> TL.Text
lazyTextFromJSString = TL.fromStrict . textFromJSString
{-# INLINE lazyTextFromJSString #-}

-- | returns the empty Text if not a string
textFromJSVal :: JSVal -> T.Text
textFromJSVal j = case js_fromString' j of
    (# _,  0#     #) -> T.empty
    (# ba, length #) -> T.Text (A.ByteArray ba) 0 (I# length)
{-# INLINE textFromJSVal #-}

-- | returns the empty Text if not a string
lazyTextFromJSVal :: JSVal -> TL.Text
lazyTextFromJSVal = TL.fromStrict . textFromJSVal
{-# INLINE lazyTextFromJSVal #-}

-- ----------------------------------------------------------------------------

#if defined(wasm32_HOST_ARCH)
-- A ByteArray# cannot be passed to JavaScript on wasm, so the UTF-8 bytes
-- are copied through a temporary buffer in the wasm memory.

-- h$textToString: decode UTF-8 bytes at (ptr, len) in the wasm memory
foreign import javascript unsafe
  "new TextDecoder().decode(new Uint8Array(__exports.memory.buffer, $1, $2))"
  js_decodeUtf8_wasm :: Ptr Word8 -> Int -> IO JSString
-- h$textFromString, first half: encode as UTF-8 (a Uint8Array)
foreign import javascript unsafe
  "new TextEncoder().encode($1)"
  js_encodeUtf8_wasm :: JSVal -> IO JSVal
foreign import javascript unsafe
  "$1.length"
  js_u8Length_wasm :: JSVal -> IO Int
-- copy a Uint8Array into the wasm memory at ptr
foreign import javascript unsafe
  "new Uint8Array(__exports.memory.buffer, $2, $1.length).set($1)"
  js_u8CopyTo_wasm :: JSVal -> Ptr Word8 -> IO ()

js_toString :: ByteArray# -> Int# -> Int# -> JSString
js_toString ba off len = unsafeDupablePerformIO $
  let t = T.Text (A.ByteArray ba) (I# off) (I# len)
  in  allocaBytes (I# len) $ \p -> do
        TF.unsafeCopyToPtr t p
        js_decodeUtf8_wasm p (I# len)
{-# NOINLINE js_toString #-}

-- the resulting Text has offset 0 (TF.fromPtr copies into a new array)
wasmTextFromString :: JSVal -> T.Text
wasmTextFromString v = unsafeDupablePerformIO $ do
  u8 <- js_encodeUtf8_wasm v
  n  <- js_u8Length_wasm u8
  allocaBytes n $ \p -> do
    js_u8CopyTo_wasm u8 p
    TF.fromPtr p (fromIntegral n)
{-# NOINLINE wasmTextFromString #-}

js_fromString :: JSString -> (# ByteArray#, Int# #)
js_fromString (JSString v) = js_fromString' v

js_fromString' :: JSVal -> (# ByteArray#, Int# #)
js_fromString' v = case wasmTextFromString v of
  T.Text (A.ByteArray ba) _ (I# len) -> (# ba, len #)

js_lazyTextToString :: Any -> JSString
js_lazyTextToString a =
  TL.foldlChunks (\acc c -> js_append_wasm acc (textToJSString c))
                 empty (unsafeCoerce a :: TL.Text)
foreign import javascript unsafe
  "$1 + $2"
  js_append_wasm :: JSString -> JSString -> JSString
#else
foreign import javascript unsafe
  "h$textToString"
  js_toString :: ByteArray# -> Int# -> Int# -> JSString
foreign import javascript unsafe
  "h$textFromString"
  js_fromString :: JSString -> (# ByteArray#, Int# #)
foreign import javascript unsafe
  "h$textFromString"
  js_fromString' :: JSVal -> (# ByteArray#, Int# #)
foreign import javascript unsafe
  "h$lazyTextToString"
  js_lazyTextToString :: Any -> JSString
#endif
