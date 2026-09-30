{-# LANGUAGE CPP #-}
{-# LANGUAGE ForeignFunctionInterface, JavaScriptFFI, UnliftedFFITypes,
             MagicHash, PolyKinds, BangPatterns
  #-}
{-|
    GHCJS implements the ByteArray# primitive with a JavaScript object
    containing an ArrayBuffer and various TypedArray views. This module
    contains utilities for manipulating and converting the buffer as
    a JavaScript object.

    None of the properties of a Buffer object should be written to in foreign
    code. Changing the contents of a MutableBuffer in foreign code is allowed.
 -}

-- fixme alignment not done yet!
module GHCJS.Buffer
    ( Buffer
    , MutableBuffer
    , create
    , createFromArrayBuffer
    , thaw, freeze, clone
      -- * JavaScript properties
    , byteLength
    , getArrayBuffer
    , getUint8Array
    , getUint16Array
    , getInt32Array
    , getDataView
    , getFloat32Array
    , getFloat64Array
      -- * primitive
    , toByteArray, fromByteArray
    , toByteArrayPrim, fromByteArrayPrim
    , toMutableByteArray, fromMutableByteArray
    , toMutableByteArrayPrim, fromMutableByteArrayPrim
      -- * bytestring
    , toByteString, fromByteString
      -- * pointers
    , toPtr, unsafeToPtr
    ) where

import GHC.Exts (ByteArray#, MutableByteArray#, Addr#, Ptr(..), Any)

import GHCJS.Buffer.Types
import GHC.JS.Prim
import GHCJS.Internal.Types

import Data.Int
import Data.Word
import Data.ByteString (ByteString)
import qualified Data.ByteString as BS
import qualified Data.ByteString.Unsafe as BS
import qualified Data.ByteString.Internal as BS
import Data.Primitive.ByteArray

import qualified JavaScript.TypedArray.Internal.Types as I
import           JavaScript.TypedArray.ArrayBuffer.Internal (SomeArrayBuffer)
import           JavaScript.TypedArray.DataView.Internal    (SomeDataView)
import qualified JavaScript.TypedArray.Internal as I

import GHC.ForeignPtr
#if defined(wasm32_HOST_ARCH)
-- the wasm JSFFI only unwraps newtypes whose constructors are in scope
import JavaScript.TypedArray.ArrayBuffer.Internal (SomeArrayBuffer(..))
import JavaScript.TypedArray.DataView.Internal (SomeDataView(..))
import JavaScript.TypedArray.Internal.Types (SomeTypedArray(..))
#endif

#if defined(wasm32_HOST_ARCH)
import Control.Monad.Primitive (touch)
import Control.Monad.ST (ST)
import Control.Monad.ST.Unsafe (unsafeIOToST, unsafeSTToIO)
import System.IO.Unsafe (unsafeDupablePerformIO, unsafePerformIO)
#endif

create :: Int -> IO MutableBuffer
create n | n >= 0    = js_create n
         | otherwise = error "create: negative size"
{-# INLINE create #-}

createFromArrayBuffer :: SomeArrayBuffer any -> SomeBuffer any
createFromArrayBuffer buf = js_wrapBuffer buf
{-# INLINE createFromArrayBuffer #-}

getArrayBuffer :: SomeBuffer any -> SomeArrayBuffer any
getArrayBuffer buf = js_getArrayBuffer buf
{-# INLINE getArrayBuffer #-}

getInt32Array :: SomeBuffer any -> I.SomeInt32Array any
getInt32Array buf = js_getInt32Array buf
{-# INLINE getInt32Array #-}

getUint8Array :: SomeBuffer any -> I.SomeUint8Array any
getUint8Array buf = js_getUint8Array buf
{-# INLINE getUint8Array #-}

getUint16Array :: SomeBuffer any -> I.SomeUint16Array any
getUint16Array buf = js_getUint16Array buf
{-# INLINE getUint16Array #-}

getFloat32Array :: SomeBuffer any -> I.SomeFloat32Array any
getFloat32Array buf = js_getFloat32Array buf
{-# INLINE getFloat32Array #-}

getFloat64Array :: SomeBuffer any -> I.SomeFloat64Array any
getFloat64Array buf = js_getFloat64Array buf
{-# INLINE getFloat64Array #-}

getDataView :: SomeBuffer any -> SomeDataView any
getDataView buf = js_getDataView buf
{-# INLINE getDataView #-}

freeze :: MutableBuffer -> IO Buffer
freeze x = js_clone x
{-# INLINE freeze #-}

thaw :: Buffer -> IO MutableBuffer
thaw buf  = js_clone buf
{-# INLINE thaw #-}

clone :: MutableBuffer -> IO (SomeBuffer any2)
clone buf = js_clone buf
{-# INLINE clone #-}

fromByteArray :: ByteArray -> Buffer
fromByteArray (ByteArray ba) = fromByteArrayPrim ba
{-# INLINE fromByteArray #-}

toByteArray :: Buffer -> ByteArray
toByteArray buf = ByteArray (toByteArrayPrim buf)
{-# INLINE toByteArray #-}

fromMutableByteArray :: MutableByteArray s -> Buffer
fromMutableByteArray (MutableByteArray mba) = fromMutableByteArrayPrim mba
{-# INLINE fromMutableByteArray #-}

#if defined(wasm32_HOST_ARCH)
{- On wasm a Haskell byte array lives in the wasm linear memory, while a
   Buffer is a JavaScript object (with the same fields as the JavaScript
   backend's byte arrays: buf, len, u8, ...) around a separate
   ArrayBuffer.  The conversions therefore COPY the bytes: later
   mutations of the source are not visible through the result (on the
   JavaScript backend both share the same memory).
 -}
fromByteArrayPrim :: ByteArray# -> Buffer
fromByteArrayPrim ba = unsafeDupablePerformIO (bufferFromByteArray (ByteArray ba))
{-# NOINLINE fromByteArrayPrim #-}

toByteArrayPrim :: Buffer -> ByteArray#
toByteArrayPrim buf =
  case unsafeDupablePerformIO (unsafeSTToIO (copyBufferToMBA buf >>= unsafeFreezeByteArray)) of
    ByteArray ba -> ba
{-# NOINLINE toByteArrayPrim #-}

-- | On wasm this takes a copy of the current contents.
fromMutableByteArrayPrim :: MutableByteArray# s -> Buffer
fromMutableByteArrayPrim mba = unsafePerformIO $ do
  ba <- unsafeSTToIO (unsafeFreezeByteArray (MutableByteArray mba))
  bufferFromByteArray ba
{-# NOINLINE fromMutableByteArrayPrim #-}

-- | On wasm this returns a fresh (pinned) copy of the buffer's contents.
toMutableByteArray :: Buffer -> MutableByteArray s
toMutableByteArray buf = unsafePerformIO (unsafeSTToIO (copyBufferToMBA buf))
{-# NOINLINE toMutableByteArray #-}

toMutableByteArrayPrim :: Buffer -> MutableByteArray# s
toMutableByteArrayPrim buf = case toMutableByteArray buf of MutableByteArray mba -> mba
{-# INLINE toMutableByteArrayPrim #-}

-- | Copy a Haskell byte array into a new Buffer.
bufferFromByteArray :: ByteArray -> IO Buffer
bufferFromByteArray ba
  | isByteArrayPinned ba = do
      ab <- js_arrayBufferFromPtr (byteArrayContents ba) n
      touch ba
      pure (js_wrapBuffer ab)
  | otherwise = do
      -- unpinned arrays may move, so copy to a pinned one first
      mba <- newPinnedByteArray n
      copyByteArray mba 0 ba 0 n
      pba <- unsafeFreezeByteArray mba
      bufferFromByteArray pba
  where n = sizeofByteArray ba

-- | Copy the whole contents of a Buffer into a new pinned byte array.
copyBufferToMBA :: SomeBuffer any -> ST s (MutableByteArray s)
copyBufferToMBA buf = do
  let n = byteLength buf
  mba <- newPinnedByteArray n
  unsafeIOToST (js_copyFromBuffer buf 0 n (mutableByteArrayContents mba))
  touch mba
  pure mba
#else
fromByteArrayPrim :: ByteArray# -> Buffer
fromByteArrayPrim ba = SomeBuffer (js_fromByteArray ba)
{-# INLINE fromByteArrayPrim #-}

toByteArrayPrim :: Buffer -> ByteArray#
toByteArrayPrim buf = js_toByteArray buf
{-# INLINE toByteArrayPrim #-}

fromMutableByteArrayPrim :: MutableByteArray# s -> Buffer
fromMutableByteArrayPrim mba = SomeBuffer (js_fromMutableByteArray mba)
{-# INLINE fromMutableByteArrayPrim #-}

toMutableByteArray :: Buffer -> MutableByteArray s
toMutableByteArray buf = MutableByteArray (toMutableByteArrayPrim buf)
{-# INLINE toMutableByteArray #-}

toMutableByteArrayPrim :: Buffer -> MutableByteArray# s
toMutableByteArrayPrim (SomeBuffer buf) = js_toMutableByteArray buf
{-# INLINE toMutableByteArrayPrim #-}
#endif

-- | Convert a 'ByteString' into a triple of (buffer, offset, length)
-- Warning: if the 'ByteString''s internal 'ForeignPtr' has a
-- finalizer associated with it, the returned 'Buffer' will not count
-- as a reference for the purpose of determining when that finalizer
-- should run.
fromByteString :: ByteString -> (Buffer, Int, Int)
#if defined(wasm32_HOST_ARCH)
-- On wasm the bytes are copied into a new Buffer, so the offset is
-- always 0 (and the finalizer caveat above does not apply).
fromByteString bs = unsafeDupablePerformIO $
  BS.unsafeUseAsCStringLen bs $ \(p, len) -> do
    ab <- js_arrayBufferFromPtr p len
    pure (js_wrapBuffer ab, 0, len)
{-# NOINLINE fromByteString #-}
#else
fromByteString (BS.BS fp len) =
  -- not super happy with this.  What if the bytestring's foreign ptr
  -- has a nontrivial finalizer attached to it?  I don't think there's
  -- a way to do that without someone else messing with the PS constructor
  -- directly though.
  let !(Ptr addr) = unsafeForeignPtrToPtr fp
      (ptr, off) = js_fromAddr addr
  in (ptr, off, len)
{-# INLINE fromByteString #-}
#endif

-- | Wrap a 'Buffer' into a 'ByteString' using the given offset
-- and length.
toByteString :: Int -> Maybe Int -> Buffer -> ByteString
toByteString off _ buf
  | off < 0                    = error "toByteString: negative offset"
  | off > byteLength buf       = error "toByteString: offset past end of buffer"
toByteString off (Just len) buf
  | len < 0                    = error "toByteString: negative length"
  | len > byteLength buf - off = error "toByteString: length past end of buffer"
  | otherwise                  = unsafeToByteString off len buf
toByteString off Nothing buf   = unsafeToByteString off (byteLength buf - off) buf

#if defined(wasm32_HOST_ARCH)
-- On wasm the bytes are copied into a new 'ByteString'.
unsafeToByteString :: Int -> Int -> Buffer -> ByteString
unsafeToByteString off len buf =
  BS.unsafeCreate len (\p -> js_copyFromBuffer buf off len p)

-- A 'Ptr' on wasm is an offset into the wasm linear memory; it cannot
-- point into a JavaScript ArrayBuffer, so these cannot be implemented.
toPtr :: MutableBuffer -> Ptr a
toPtr _ = error "GHCJS.Buffer.toPtr: not supported on the wasm backend"
{-# NOINLINE toPtr #-}

unsafeToPtr :: Buffer -> Ptr a
unsafeToPtr _ = error "GHCJS.Buffer.unsafeToPtr: not supported on the wasm backend"
{-# NOINLINE unsafeToPtr #-}
#else
unsafeToByteString :: Int -> Int -> Buffer -> ByteString
unsafeToByteString off len buf@(SomeBuffer bufRef) =
  let fp = ForeignPtr (js_toAddr buf) (PlainPtr (js_toMutableByteArray bufRef))
  in BS.PS fp off len

toPtr :: MutableBuffer -> Ptr a
toPtr buf = Ptr (js_toAddr buf)
{-# INLINE toPtr #-}

unsafeToPtr :: Buffer -> Ptr a
unsafeToPtr buf = Ptr (js_toAddr buf)
{-# INLINE unsafeToPtr #-}
#endif

byteLength :: SomeBuffer any -> Int
byteLength buf = js_byteLength buf
{-# INLINE byteLength #-}

-- ----------------------------------------------------------------------------

#if defined(wasm32_HOST_ARCH)
-- A Buffer is a JavaScript object with the same fields as the
-- JavaScript backend's byte arrays (see h$newByteArray and h$wrapBuffer
-- in GHC's rts/js/mem.js), so the property accessors below work
-- unchanged.

-- h$newByteArray
foreign import javascript unsafe
  "var n = $1; var r = n % 8; var b = new ArrayBuffer(Math.max(r === 0 ? n : n - r + 8, 8)); return { buf: b, len: n, i3: new Int32Array(b), u8: new Uint8Array(b), u1: new Uint16Array(b), f3: new Float32Array(b), f6: new Float64Array(b), dv: new DataView(b), arr: [], m: 0 };"
  js_create :: Int -> IO MutableBuffer
-- h$wrapBuffer(buf) (offset 0, whole buffer)
foreign import javascript unsafe
  "var b = $1; if(!b || !(b instanceof ArrayBuffer)) { throw new Error('h$wrapBuffer: not an ArrayBuffer: ' + b); } var n = b.byteLength; return { buf: b, len: n, i3: new Int32Array(b, 0, n >> 2), u8: new Uint8Array(b, 0, n), u1: new Uint16Array(b, 0, n >> 1), f3: new Float32Array(b, 0, n >> 2), f6: new Float64Array(b, 0, n >> 3), dv: new DataView(b, 0, n) };"
  js_wrapBuffer :: SomeArrayBuffer any -> SomeBuffer any
foreign import javascript unsafe
  "$1.buf.slice($1.u8.byteOffset, $1.len)"
  js_cloneArrayBuffer :: SomeBuffer any1 -> IO (SomeArrayBuffer any2)

js_clone :: SomeBuffer any1 -> IO (SomeBuffer any2)
js_clone x = do
  ab <- js_cloneArrayBuffer x
  pure $! js_wrapBuffer ab

-- | copy len bytes starting at the given offset in the wasm memory into
--   a new ArrayBuffer
foreign import javascript unsafe
  "__exports.memory.buffer.slice($1, $1 + $2)"
  js_arrayBufferFromPtr :: Ptr a -> Int -> IO (SomeArrayBuffer any)
-- | js_copyFromBuffer buf off len ptr: copy len bytes starting at offset
--   off in buf to the wasm memory at ptr
foreign import javascript unsafe
  "new Uint8Array(__exports.memory.buffer, $4, $3).set($1.u8.subarray($2, $2 + $3));"
  js_copyFromBuffer :: SomeBuffer any -> Int -> Int -> Ptr a -> IO ()
#else
foreign import javascript unsafe
  "h$newByteArray" js_create :: Int -> IO MutableBuffer
foreign import javascript unsafe
  "h$wrapBuffer" js_wrapBuffer :: SomeArrayBuffer any -> SomeBuffer any
foreign import javascript unsafe
  "((x) => { return h$wrapBuffer(x.buf.slice(x.u8.byteOffset, x.len)); })"
  js_clone :: SomeBuffer any1 -> IO (SomeBuffer any2)
#endif
#if defined(wasm32_HOST_ARCH)
foreign import javascript unsafe "((x) => { return x.len; })($1)" js_byteLength :: SomeBuffer any -> Int
#else
foreign import javascript unsafe
  "((x) => { return x.len; })" js_byteLength :: SomeBuffer any -> Int
#endif
#if defined(wasm32_HOST_ARCH)
foreign import javascript unsafe "((x) => { return x.buf; })($1)" js_getArrayBuffer    :: SomeBuffer any -> SomeArrayBuffer any
#else
foreign import javascript unsafe
  "((x) => { return x.buf; })" js_getArrayBuffer    :: SomeBuffer any -> SomeArrayBuffer any
#endif
#if defined(wasm32_HOST_ARCH)
foreign import javascript unsafe "((x) => { return x.i3; })($1)" js_getInt32Array      :: SomeBuffer any -> I.SomeInt32Array any
#else
foreign import javascript unsafe
  "((x) => { return x.i3; })" js_getInt32Array      :: SomeBuffer any -> I.SomeInt32Array any
#endif
#if defined(wasm32_HOST_ARCH)
foreign import javascript unsafe "((x) => { return x.u8; })($1)" js_getUint8Array      :: SomeBuffer any -> I.SomeUint8Array  any
#else
foreign import javascript unsafe
  "((x) => { return x.u8; })" js_getUint8Array      :: SomeBuffer any -> I.SomeUint8Array  any
#endif
#if defined(wasm32_HOST_ARCH)
foreign import javascript unsafe "((x) => { return x.u1; })($1)" js_getUint16Array     :: SomeBuffer any -> I.SomeUint16Array any
#else
foreign import javascript unsafe
  "((x) => { return x.u1; })" js_getUint16Array     :: SomeBuffer any -> I.SomeUint16Array any
#endif
#if defined(wasm32_HOST_ARCH)
foreign import javascript unsafe "((x) => { return x.f3; })($1)" js_getFloat32Array    :: SomeBuffer any -> I.SomeFloat32Array  any
#else
foreign import javascript unsafe
  "((x) => { return x.f3; })" js_getFloat32Array    :: SomeBuffer any -> I.SomeFloat32Array  any
#endif
#if defined(wasm32_HOST_ARCH)
foreign import javascript unsafe "((x) => { return x.f6; })($1)" js_getFloat64Array    :: SomeBuffer any -> I.SomeFloat64Array any
#else
foreign import javascript unsafe
  "((x) => { return x.f6; })" js_getFloat64Array    :: SomeBuffer any -> I.SomeFloat64Array any
#endif
#if defined(wasm32_HOST_ARCH)
foreign import javascript unsafe "((x) => { return x.dv; })($1)" js_getDataView        :: SomeBuffer any -> SomeDataView any
#else
foreign import javascript unsafe
  "((x) => { return x.dv; })" js_getDataView        :: SomeBuffer any -> SomeDataView any
#endif

-- (On wasm Haskell byte arrays and Buffers do not share a representation;
-- see the copying conversions above.)
#if !defined(wasm32_HOST_ARCH)
-- ----------------------------------------------------------------------------
-- these things have the same representation (modulo boxing),
-- conversion is free

foreign import javascript unsafe  
  "((x) => { return x; })" js_toByteArray          :: SomeBuffer any      -> ByteArray#
foreign import javascript unsafe  
  "((x) => { return x; })" js_fromByteArray        :: ByteArray#          -> JSVal
foreign import javascript unsafe
  "((x) => { return x; })" js_fromMutableByteArray :: MutableByteArray# s -> JSVal
foreign import javascript unsafe
  "((x) => { return x; })" js_toMutableByteArray   :: JSVal               -> MutableByteArray# s
foreign import javascript unsafe
  "h$toAddr"               js_toAddr               :: SomeBuffer any      -> Addr#
foreign import javascript unsafe
  "h$fromAddr"             js_fromAddr             :: Addr#               -> (SomeBuffer any, Int)
#endif
