{-# LANGUAGE CPP #-}
{-# LANGUAGE MagicHash #-}
{-# LANGUAGE UnboxedTuples #-}
{-# LANGUAGE JavaScriptFFI #-}
{-# LANGUAGE ForeignFunctionInterface #-}
{-# LANGUAGE UnliftedFFITypes #-}
{-# LANGUAGE GHCForeignImportPrim #-}
{-# LANGUAGE EmptyDataDecls #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeSynonymInstances #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE PolyKinds #-}
{-# LANGUAGE KindSignatures #-}

module JavaScript.TypedArray.Internal where

import Data.Typeable

import GHC.Types
import GHC.Exts
import GHC.ST
#if defined(wasm32_HOST_ARCH)
import GHC.IO (unsafeIOToST)
#endif

import GHC.Int
import GHC.Word

import GHCJS.Internal.Types
import GHCJS.Buffer.Types
import GHCJS.Types

import JavaScript.Array.Internal (SomeJSArray(..), JSArray)
import JavaScript.TypedArray.ArrayBuffer
import JavaScript.TypedArray.ArrayBuffer.Internal (SomeArrayBuffer(..))
import JavaScript.TypedArray.Internal.Types

elemSize :: SomeTypedArray e m -> Int
elemSize a = js_elemSize a
{-# INLINE [1] elemSize #-}
{-# RULES "elemSizeUint8Clamped" forall (x :: SomeUint8ClampedArray m). elemSize x = 1 #-}
{-# RULES "elemSizeUint8"        forall (x :: SomeUint8Array m).        elemSize x = 1 #-}
{-# RULES "elemSizeUint16"       forall (x :: SomeUint16Array m).       elemSize x = 2 #-}
{-# RULES "elemSizeUint32"       forall (x :: SomeUint32Array m).       elemSize x = 4 #-}
{-# RULES "elemSizeInt8"         forall (x :: SomeInt8Array m).         elemSize x = 1 #-}
{-# RULES "elemSizeInt16"        forall (x :: SomeInt16Array m).        elemSize x = 2 #-}
{-# RULES "elemSizeInt32"        forall (x :: SomeInt32Array m).        elemSize x = 4 #-}
{-# RULES "elemSizeFloat32"      forall (x :: SomeFloat32Array m).      elemSize x = 4 #-}
{-# RULES "elemSizeFloat64"      forall (x :: SomeFloat64Array m).      elemSize x = 8 #-}

instance TypedArray IOInt8Array where
  index i a                  = IO (indexI8 i a)
  unsafeIndex i a            = IO (unsafeIndexI8 i a)
  setIndex i (I8# x) a       = IO (setIndexI i (int8ToInt# x) a)
  unsafeSetIndex i (I8# x) a = IO (unsafeSetIndexI i (int8ToInt# x) a)
  indexOf s (I8# x) a        = IO (indexOfI s (int8ToInt# x) a)
  lastIndexOf s (I8# x) a    = IO (lastIndexOfI s (int8ToInt# x) a)
  create l                   = IO (js_createInt8Array l)
  fromArray a                = int8ArrayFrom a
  fromArrayBuffer b          = undefined

instance TypedArray IOInt16Array where
  index i a                   = IO (indexI16 i a)
  unsafeIndex i a             = IO (unsafeIndexI16 i a)
  setIndex i (I16# x) a       = IO (setIndexI i (int16ToInt# x) a)
  unsafeSetIndex i (I16# x) a = IO (unsafeSetIndexI i (int16ToInt# x) a)
  indexOf s (I16# x) a        = IO (indexOfI s (int16ToInt# x) a)
  lastIndexOf s (I16# x) a    = IO (lastIndexOfI s (int16ToInt# x) a)
  create l                    = IO (js_createInt16Array l)
  fromArray a                 = int16ArrayFrom a
  fromArrayBuffer b           = undefined

instance TypedArray IOInt32Array where
  index i a                   = IO (indexI i a)
  unsafeIndex i a             = IO (unsafeIndexI i a)
  setIndex i (I# x) a         = IO (setIndexI i x a)
  unsafeSetIndex i (I# x) a   = IO (unsafeSetIndexI i x a)
  indexOf s (I# x) a          = IO (indexOfI s x a)
  lastIndexOf s (I# x) a      = IO (lastIndexOfI s x a)
  create l                    = IO (js_createInt32Array l)
  fromArray a                 = int32ArrayFrom a
  fromArrayBuffer b           = undefined

instance TypedArray IOUint8ClampedArray where
  index i a                   = IO (indexW8 i a)
  unsafeIndex i a             = IO (unsafeIndexW8 i a)
  setIndex i (W8# x) a        = IO (setIndexW i (word8ToWord# x) a)
  unsafeSetIndex i (W8# x) a  = IO (unsafeSetIndexW i (word8ToWord# x) a)
  indexOf s (W8# x) a         = IO (indexOfW s (word8ToWord# x) a)
  lastIndexOf s (W8# x) a     = IO (lastIndexOfW s (word8ToWord# x) a)
  create l                    = IO (js_createUint8ClampedArray l)
  fromArray a                 = uint8ClampedArrayFrom a
  fromArrayBuffer b           = undefined

instance TypedArray IOUint8Array where
  index i a                   = IO (indexW8 i a)
  unsafeIndex i a             = IO (unsafeIndexW8 i a)
  setIndex i (W8# x) a        = IO (setIndexW i (word8ToWord# x) a)
  unsafeSetIndex i (W8# x) a  = IO (unsafeSetIndexW i (word8ToWord# x) a)
  indexOf s (W8# x) a         = IO (indexOfW s (word8ToWord# x) a)
  lastIndexOf s (W8# x) a     = IO (lastIndexOfW s (word8ToWord# x) a)
  create l                    = IO (js_createUint8Array l)
  fromArray a                 = uint8ArrayFrom a
  fromArrayBuffer b           = undefined

instance TypedArray IOUint16Array where
  index i a                   = IO (indexW16 i a)
  unsafeIndex i a             = IO (unsafeIndexW16 i a)
  setIndex i (W16# x) a       = IO (setIndexW i (word16ToWord# x) a)
  unsafeSetIndex i (W16# x) a = IO (unsafeSetIndexW i (word16ToWord# x) a)
  indexOf s (W16# x) a        = IO (indexOfW s (word16ToWord# x) a)
  lastIndexOf s (W16# x) a    = IO (lastIndexOfW s (word16ToWord# x) a)
  create l                    = IO (js_createUint16Array l)
  fromArray a                 = uint16ArrayFrom a
  fromArrayBuffer b           = undefined

instance TypedArray IOUint32Array where
  index i a                   = IO (indexW i a)
  unsafeIndex i a             = IO (unsafeIndexW i a)
  setIndex i (W# x) a         = IO (setIndexW i x a)
  unsafeSetIndex i (W# x) a   = IO (unsafeSetIndexW i x a)
  indexOf s (W# x) a          = IO (indexOfW s x a)
  lastIndexOf s (W# x) a      = IO (lastIndexOfW s x a)
  create l                    = IO (js_createUint32Array l)
  fromArray a                 = uint32ArrayFrom a
  fromArrayBuffer b           = undefined

instance TypedArray IOFloat32Array where
  index i a                   = IO (indexD i a)
  unsafeIndex i a             = IO (unsafeIndexD i a)
  setIndex i x a              = IO (setIndexD i x a)
  unsafeSetIndex i x a        = IO (unsafeSetIndexD i x a)
  indexOf s x a               = IO (indexOfD s x a)
  lastIndexOf s x a           = IO (lastIndexOfD s x a)
  create l                    = IO (js_createFloat32Array l)
  fromArray a                 = float32ArrayFrom a
  fromArrayBuffer b           = undefined

instance TypedArray IOFloat64Array where
  index i a                   = IO (indexD i a)
  unsafeIndex i a             = IO (unsafeIndexD i a)
  setIndex i x a              = IO (setIndexD i x a)
  unsafeSetIndex i x a        = IO (unsafeSetIndexD i x a)
  indexOf s x a               = IO (indexOfD s x a)
  lastIndexOf s x a           = IO (lastIndexOfD s x a)
  create l                    = IO (js_createFloat64Array l)
  fromArray a                 = float64ArrayFrom a
  fromArrayBuffer b           = undefined


class TypedArray a where
  unsafeIndex     :: Int           -> a -> IO (Elem a)
  index           :: Int           -> a -> IO (Elem a)
  unsafeSetIndex  :: Int -> Elem a -> a -> IO ()
  setIndex        :: Int -> Elem a -> a -> IO ()
  create          :: Int                -> IO a
  fromArray       :: SomeJSArray m      -> IO a
  fromArrayBuffer :: MutableArrayBuffer -> Int    -> Maybe Int -> IO a
  indexOf         :: Int                -> Elem a -> a -> IO Int
  lastIndexOf     :: Int                -> Elem a -> a -> IO Int

-- -----------------------------------------------------------------------------

indexI :: Int -> SomeTypedArray e m -> State# s -> (# State# s, Int #)
indexI a i = \s -> case js_indexI a i s of (# s', v #) -> (# s', I# v #)
{-# INLINE indexI #-}

indexI16 :: Int -> SomeTypedArray e m -> State# s -> (# State# s, Int16 #)
indexI16 a i = \s -> case js_indexI a i s of (# s', v #) -> (# s', I16# (intToInt16# v) #)
{-# INLINE indexI16 #-}

indexI8 :: Int -> SomeTypedArray e m -> State# s -> (# State# s, Int8 #)
indexI8 a i = \s -> case js_indexI a i s of (# s', v #) -> (# s', I8# (intToInt8# v) #)
{-# INLINE indexI8 #-}

indexW :: Int -> SomeTypedArray e m -> State# s -> (# State# s, Word #)
indexW a i = \s -> case js_indexW a i s of (# s', v #) -> (# s', W# v #)
{-# INLINE indexW #-}

indexW16 :: Int -> SomeTypedArray e m -> State# s -> (# State# s, Word16 #)
indexW16 a i = \s -> case js_indexW a i s of (# s', v #) -> (# s', W16# (wordToWord16# v) #)
{-# INLINE indexW16 #-}

indexW8 :: Int -> SomeTypedArray e m -> State# s -> (# State# s,  Word8 #)
indexW8 a i = \s -> case js_indexW a i s of (# s', v #) -> (# s', W8# (wordToWord8# v) #)
{-# INLINE indexW8 #-}

indexD :: Int -> SomeTypedArray e m -> State# s -> (# State# s, Double #)
indexD a i = \s -> js_indexD a i s
{-# INLINE indexD #-}

-- -----------------------------------------------------------------------------

unsafeIndexI :: Int -> SomeTypedArray e m -> State# s -> (# State# s, Int #)
unsafeIndexI a i = \s -> case js_unsafeIndexI a i s of (# s', v #) -> (# s', I# v #)
{-# INLINE unsafeIndexI #-}

unsafeIndexI16 :: Int -> SomeTypedArray e m -> State# s -> (# State# s, Int16 #)
unsafeIndexI16 a i = \s -> case js_unsafeIndexI a i s of (# s', v #) -> (# s', I16# (intToInt16# v) #)
{-# INLINE unsafeIndexI16 #-}

unsafeIndexI8 :: Int -> SomeTypedArray e m -> State# s -> (# State# s, Int8 #)
unsafeIndexI8 a i = \s -> case js_unsafeIndexI a i s of (# s', v #) -> (# s', I8# (intToInt8# v) #)
{-# INLINE unsafeIndexI8 #-}

unsafeIndexW :: Int -> SomeTypedArray e m -> State# s -> (# State# s,  Word #)
unsafeIndexW a i = \s -> case js_unsafeIndexW a i s of (# s', v #) -> (# s', W# v #)
{-# INLINE unsafeIndexW #-}

unsafeIndexW16 :: Int -> SomeTypedArray e m -> State# s -> (# State# s, Word16 #)
unsafeIndexW16 a i = \s -> case js_unsafeIndexW a i s of (# s', v #) -> (# s', W16# (wordToWord16# v) #)
{-# INLINE unsafeIndexW16 #-}

unsafeIndexW8 :: Int -> SomeTypedArray e m -> State# s -> (# State# s, Word8 #)
unsafeIndexW8 a i = \s -> case js_unsafeIndexW a i s of (# s', v #) -> (# s', W8# (wordToWord8# v) #)
{-# INLINE unsafeIndexW8 #-}

unsafeIndexD :: Int -> SomeTypedArray e m -> State# s -> (# State# s,  Double #)
unsafeIndexD a i = \s -> js_unsafeIndexD a i s
{-# INLINE unsafeIndexD #-}

-- -----------------------------------------------------------------------------

int8ArrayFrom :: SomeJSArray m0 -> IO (SomeInt8Array m1)
int8ArrayFrom a = js_int8ArrayFromArray a
{-# INLINE int8ArrayFrom #-}

int16ArrayFrom :: SomeJSArray m0 -> IO (SomeInt16Array m1)
int16ArrayFrom a = js_int16ArrayFromArray a
{-# INLINE int16ArrayFrom #-}

int32ArrayFrom :: SomeJSArray m0 -> IO (SomeInt32Array m1)
int32ArrayFrom a = js_int32ArrayFromArray a
{-# INLINE int32ArrayFrom #-}

uint8ArrayFrom :: SomeJSArray m0 -> IO (SomeUint8Array m1)
uint8ArrayFrom a = js_uint8ArrayFromArray a
{-# INLINE uint8ArrayFrom #-}

uint8ClampedArrayFrom :: SomeJSArray m0 -> IO (SomeUint8ClampedArray m1)
uint8ClampedArrayFrom a = js_uint8ClampedArrayFromArray a
{-# INLINE uint8ClampedArrayFrom #-}

uint16ArrayFrom :: SomeJSArray m0 -> IO (SomeUint16Array m1)
uint16ArrayFrom a = js_uint16ArrayFromArray a
{-# INLINE uint16ArrayFrom #-}

uint32ArrayFrom :: SomeJSArray m0 -> IO (SomeUint32Array m1)
uint32ArrayFrom a = js_uint32ArrayFromArray a
{-# INLINE uint32ArrayFrom #-}

float32ArrayFrom :: SomeJSArray m0 -> IO (SomeFloat32Array m1)
float32ArrayFrom a = js_float32ArrayFromArray a
{-# INLINE float32ArrayFrom #-}

float64ArrayFrom :: SomeJSArray m0 -> IO (SomeFloat64Array m1)
float64ArrayFrom a = js_float64ArrayFromArray a
{-# INLINE float64ArrayFrom #-}

-- -----------------------------------------------------------------------------

setIndexI :: Mutability m ~ IsMutable
          => Int -> Int# -> SomeTypedArray e m -> State# s -> (# State# s, () #)
setIndexI i x a = js_setIndexI i x a
{-# INLINE setIndexI #-}

unsafeSetIndexI :: Mutability m ~ IsMutable
                => Int -> Int# -> SomeTypedArray e m -> State# s -> (# State# s, () #)
unsafeSetIndexI i x a = js_unsafeSetIndexI i x a
{-# INLINE unsafeSetIndexI #-}

setIndexW :: Mutability m ~ IsMutable
           => Int -> Word# -> SomeTypedArray e m -> State# s -> (# State# s, () #)
setIndexW i x a = js_setIndexW i x a
{-# INLINE setIndexW #-}

unsafeSetIndexW :: Mutability m ~ IsMutable
                => Int -> Word# -> SomeTypedArray e m -> State# s -> (# State# s, () #)
unsafeSetIndexW i x a = js_unsafeSetIndexW i x a
{-# INLINE unsafeSetIndexW #-}

setIndexD :: Mutability m ~ IsMutable
          => Int -> Double -> SomeTypedArray e m -> State# s -> (# State# s, () #)
setIndexD i x a = js_setIndexD i x a
{-# INLINE setIndexD #-}

unsafeSetIndexD :: Mutability m ~ IsMutable
                => Int -> Double -> SomeTypedArray e m -> State# s -> (# State# s, () #)
unsafeSetIndexD i x a = js_unsafeSetIndexD i x a
{-# INLINE unsafeSetIndexD #-}

indexOfI :: Mutability m ~ IsMutable
         => Int -> Int# -> SomeTypedArray e m -> State# s -> (# State# s, Int #)
indexOfI s x a = js_indexOfI s x a
{-# INLINE indexOfI #-}

indexOfW :: Int -> Word# -> SomeTypedArray e m -> State# s -> (# State# s, Int #)
indexOfW s x a = js_indexOfW s x a
{-# INLINE indexOfW #-}

indexOfD :: Int -> Double -> SomeTypedArray e m -> State# s -> (# State# s, Int #)
indexOfD s x a = js_indexOfD s x a
{-# INLINE indexOfD #-}

lastIndexOfI :: Int -> Int# -> SomeTypedArray e m -> State# s -> (# State# s, Int #)
lastIndexOfI s x a = js_lastIndexOfI s x a
{-# INLINE lastIndexOfI #-}

lastIndexOfW :: Int -> Word# -> SomeTypedArray e m -> State# s -> (# State# s, Int #)
lastIndexOfW s x a = js_lastIndexOfW s x a
{-# INLINE lastIndexOfW #-}

lastIndexOfD :: Int -> Double -> SomeTypedArray e m -> State# s -> (# State# s, Int #)
lastIndexOfD s x a = js_lastIndexOfD s x a
{-# INLINE lastIndexOfD #-}

-- -----------------------------------------------------------------------------
-- non-class operations usable for all typed array
{-| length of the typed array in elements -}
length :: SomeTypedArray e m -> Int
length x = js_length x
{-# INLINE length #-}

{-| length of the array in bytes -}
byteLength :: SomeTypedArray e m -> Int
byteLength x = js_byteLength x
{-# INLINE byteLength #-}

{-| offset of the array in the buffer -}
byteOffset :: SomeTypedArray e m -> Int
byteOffset x = js_byteOffset x
{-# INLINE byteOffset #-}

{-| the underlying buffer of the array #-}
buffer :: SomeTypedArray e m -> SomeArrayBuffer m
buffer x = js_buffer x
{-# INLINE buffer #-}

{-| create a view of the existing array -}
subarray :: Int -> Int -> SomeTypedArray e m -> SomeTypedArray e m
subarray begin end x = js_subarray begin end x
{-# INLINE subarray #-}

-- fixme convert JSException to Haskell exception
{-| copy the elements of one typed array to another -}
set :: Int -> SomeTypedArray e m -> SomeTypedArray e1 Mutable -> IO ()
set offset src dest = IO (js_set offset src dest)
{-# INLINE set #-}

unsafeSet :: Int -> SomeTypedArray e m -> SomeTypedArray e1 Mutable -> IO ()
unsafeSet offset src dest = IO (js_unsafeSet offset src dest)
{-# INLINE unsafeSet #-}

-- -----------------------------------------------------------------------------

#if defined(wasm32_HOST_ARCH)
foreign import javascript unsafe "((x) => { return x.length; })($1)" js_length :: SomeTypedArray e m -> Int
#else
foreign import javascript unsafe
  "((x) => { return x.length; })" js_length :: SomeTypedArray e m -> Int
#endif
#if defined(wasm32_HOST_ARCH)
foreign import javascript unsafe "((x) => { return x.byteLength; })($1)" js_byteLength :: SomeTypedArray e m -> Int
#else
foreign import javascript unsafe
  "((x) => { return x.byteLength; })" js_byteLength :: SomeTypedArray e m -> Int
#endif
#if defined(wasm32_HOST_ARCH)
foreign import javascript unsafe "((x) => { return x.byteOffset; })($1)" js_byteOffset :: SomeTypedArray e m -> Int
#else
foreign import javascript unsafe
  "((x) => { return x.byteOffset; })" js_byteOffset :: SomeTypedArray e m -> Int
#endif
#if defined(wasm32_HOST_ARCH)
foreign import javascript unsafe "((x) => { return x.buffer; })($1)" js_buffer :: SomeTypedArray e m -> SomeArrayBuffer m
#else
foreign import javascript unsafe
  "((x) => { return x.buffer; })" js_buffer :: SomeTypedArray e m -> SomeArrayBuffer m
#endif
#if defined(wasm32_HOST_ARCH)
foreign import javascript unsafe "((x,y,z) => { return z.subarray(x,y); })($1,$2,$3)"
#else
foreign import javascript unsafe
  "((x,y,z) => { return z.subarray(x,y); })"
#endif
  js_subarray :: Int -> Int -> SomeTypedArray e m -> SomeTypedArray e m
#if defined(wasm32_HOST_ARCH)
-- wasm: the State#-threaded JavaScript-backend imports become boxed IO
-- imports (all synchronous, so 'unsafe') plus wrappers keeping the
-- original names and types.  NB: the argument order of z.set(x,y) is
-- kept exactly as in the JavaScript-backend code.
foreign import javascript unsafe "$3.set($1,$2);"
  js_set_wasm :: Int -> SomeTypedArray e m -> SomeTypedArray e1 m1 -> IO ()
js_set :: Int -> SomeTypedArray e m -> SomeTypedArray e1 m1 -> State# s ->  (# State# s, () #)
js_set o x y s = case unsafeIOToST (js_set_wasm o x y) of ST f -> f s
js_unsafeSet :: Int -> SomeTypedArray e m -> SomeTypedArray e1 m1 -> State# s -> (# State# s, () #)
js_unsafeSet o x y s = case unsafeIOToST (js_set_wasm o x y) of ST f -> f s
#else
foreign import javascript safe
  "((x,y,z) => { z.set(x,y); })"
  js_set :: Int -> SomeTypedArray e m -> SomeTypedArray e1 m1 -> State# s ->  (# State# s, () #)
foreign import javascript unsafe
  "((x,y,z) => { z.set(x,y); })"
  js_unsafeSet :: Int -> SomeTypedArray e m -> SomeTypedArray e1 m1 -> State# s -> (# State# s, () #)
#endif
#if defined(wasm32_HOST_ARCH)
foreign import javascript unsafe "((x) => { return x.BYTES_PER_ELEMENT; })($1)"
#else
foreign import javascript unsafe
  "((x) => { return x.BYTES_PER_ELEMENT; })"
#endif
  js_elemSize :: SomeTypedArray e m -> Int

-- -----------------------------------------------------------------------------
-- index

#if defined(wasm32_HOST_ARCH)
foreign import javascript unsafe "$2[$1]" js_indexI_wasm :: Int -> SomeTypedArray e m -> IO Int
foreign import javascript unsafe "$2[$1]" js_indexW_wasm :: Int -> SomeTypedArray e m -> IO Word
foreign import javascript unsafe "$2[$1]" js_indexD_wasm :: Int -> SomeTypedArray e m -> IO Double

js_indexI :: Int -> SomeTypedArray e m -> State# s -> (# State# s, Int# #)
js_indexI i a s = case unsafeIOToST (js_indexI_wasm i a) of
  ST f -> case f s of (# s', I# v #) -> (# s', v #)
js_indexW :: Int -> SomeTypedArray e m -> State# s -> (# State# s, Word# #)
js_indexW i a s = case unsafeIOToST (js_indexW_wasm i a) of
  ST f -> case f s of (# s', W# v #) -> (# s', v #)
js_indexD :: Int -> SomeTypedArray e m -> State# s -> (# State# s, Double #)
js_indexD i a s = case unsafeIOToST (js_indexD_wasm i a) of ST f -> f s

js_unsafeIndexI :: Int -> SomeTypedArray e m -> State# s -> (# State# s, Int# #)
js_unsafeIndexI = js_indexI
js_unsafeIndexW :: Int -> SomeTypedArray e m -> State# s -> (# State# s, Word# #)
js_unsafeIndexW = js_indexW
js_unsafeIndexD :: Int -> SomeTypedArray e m -> State# s -> (# State# s, Double #)
js_unsafeIndexD = js_indexD

-- setIndex

foreign import javascript unsafe "$3[$1] = $2;" js_setIndexI_wasm :: Int -> Int    -> SomeTypedArray e m -> IO ()
foreign import javascript unsafe "$3[$1] = $2;" js_setIndexW_wasm :: Int -> Word   -> SomeTypedArray e m -> IO ()
foreign import javascript unsafe "$3[$1] = $2;" js_setIndexD_wasm :: Int -> Double -> SomeTypedArray e m -> IO ()

js_setIndexI :: Int -> Int# -> SomeTypedArray e m -> State# s -> (# State# s, () #)
js_setIndexI i x a s = case unsafeIOToST (js_setIndexI_wasm i (I# x) a) of ST f -> f s
js_setIndexW :: Int -> Word# -> SomeTypedArray e m -> State# s -> (# State# s, () #)
js_setIndexW i x a s = case unsafeIOToST (js_setIndexW_wasm i (W# x) a) of ST f -> f s
js_setIndexD :: Int -> Double -> SomeTypedArray e m -> State# s -> (# State# s, () #)
js_setIndexD i x a s = case unsafeIOToST (js_setIndexD_wasm i x a) of ST f -> f s

js_unsafeSetIndexI :: Int -> Int# -> SomeTypedArray e m -> State# s -> (# State# s, () #)
js_unsafeSetIndexI = js_setIndexI
js_unsafeSetIndexW :: Int -> Word# -> SomeTypedArray e m -> State# s -> (# State# s, () #)
js_unsafeSetIndexW = js_setIndexW
js_unsafeSetIndexD :: Int -> Double -> SomeTypedArray e m -> State# s -> (# State# s, () #)
js_unsafeSetIndexD = js_setIndexD

-- indexOf / lastIndexOf

foreign import javascript unsafe "$3.indexOf($2,$1)" js_indexOfI_wasm :: Int -> Int    -> SomeTypedArray e m -> IO Int
foreign import javascript unsafe "$3.indexOf($2,$1)" js_indexOfW_wasm :: Int -> Word   -> SomeTypedArray e m -> IO Int
foreign import javascript unsafe "$3.indexOf($2,$1)" js_indexOfD_wasm :: Int -> Double -> SomeTypedArray e m -> IO Int
foreign import javascript unsafe "$3.lastIndexOf($2,$1)" js_lastIndexOfI_wasm :: Int -> Int    -> SomeTypedArray e m -> IO Int
foreign import javascript unsafe "$3.lastIndexOf($2,$1)" js_lastIndexOfW_wasm :: Int -> Word   -> SomeTypedArray e m -> IO Int
foreign import javascript unsafe "$3.lastIndexOf($2,$1)" js_lastIndexOfD_wasm :: Int -> Double -> SomeTypedArray e m -> IO Int

js_indexOfI :: Int -> Int#   -> SomeTypedArray e m -> State# s -> (# State# s, Int #)
js_indexOfI i x a s = case unsafeIOToST (js_indexOfI_wasm i (I# x) a) of ST f -> f s
js_indexOfW :: Int -> Word#  -> SomeTypedArray e m -> State# s -> (# State# s, Int #)
js_indexOfW i x a s = case unsafeIOToST (js_indexOfW_wasm i (W# x) a) of ST f -> f s
js_indexOfD :: Int -> Double -> SomeTypedArray e m -> State# s -> (# State# s, Int #)
js_indexOfD i x a s = case unsafeIOToST (js_indexOfD_wasm i x a) of ST f -> f s
js_lastIndexOfI :: Int -> Int# -> SomeTypedArray e m -> State# s -> (# State# s, Int #)
js_lastIndexOfI i x a s = case unsafeIOToST (js_lastIndexOfI_wasm i (I# x) a) of ST f -> f s
js_lastIndexOfW :: Int -> Word# -> SomeTypedArray e m -> State# s -> (# State# s, Int #)
js_lastIndexOfW i x a s = case unsafeIOToST (js_lastIndexOfW_wasm i (W# x) a) of ST f -> f s
js_lastIndexOfD :: Int -> Double -> SomeTypedArray e m -> State# s -> (# State# s, Int #)
js_lastIndexOfD i x a s = case unsafeIOToST (js_lastIndexOfD_wasm i x a) of ST f -> f s

-- create

foreign import javascript unsafe "new Int8Array($1)"         js_createInt8Array_wasm         :: Int -> IO (SomeInt8Array m)
foreign import javascript unsafe "new Int16Array($1)"        js_createInt16Array_wasm        :: Int -> IO (SomeInt16Array m)
foreign import javascript unsafe "new Int32Array($1)"        js_createInt32Array_wasm        :: Int -> IO (SomeInt32Array m)
foreign import javascript unsafe "new Uint8ClampedArray($1)" js_createUint8ClampedArray_wasm :: Int -> IO (SomeUint8ClampedArray m)
foreign import javascript unsafe "new Uint8Array($1)"        js_createUint8Array_wasm        :: Int -> IO (SomeUint8Array m)
foreign import javascript unsafe "new Uint16Array($1)"       js_createUint16Array_wasm       :: Int -> IO (SomeUint16Array m)
foreign import javascript unsafe "new Uint32Array($1)"       js_createUint32Array_wasm       :: Int -> IO (SomeUint32Array m)
foreign import javascript unsafe "new Float32Array($1)"      js_createFloat32Array_wasm      :: Int -> IO (SomeFloat32Array m)
foreign import javascript unsafe "new Float64Array($1)"      js_createFloat64Array_wasm      :: Int -> IO (SomeFloat64Array m)

js_createInt8Array         :: Int -> State# s -> (# State# s,  SomeInt8Array m #)
js_createInt8Array n s = case unsafeIOToST (js_createInt8Array_wasm n) of ST f -> f s
js_createInt16Array        :: Int -> State# s -> (# State# s,  SomeInt16Array m #)
js_createInt16Array n s = case unsafeIOToST (js_createInt16Array_wasm n) of ST f -> f s
js_createInt32Array        :: Int -> State# s -> (# State# s,  SomeInt32Array m #)
js_createInt32Array n s = case unsafeIOToST (js_createInt32Array_wasm n) of ST f -> f s
js_createUint8ClampedArray :: Int -> State# s -> (# State# s,  SomeUint8ClampedArray m #)
js_createUint8ClampedArray n s = case unsafeIOToST (js_createUint8ClampedArray_wasm n) of ST f -> f s
js_createUint8Array        :: Int -> State# s -> (# State# s,  SomeUint8Array m #)
js_createUint8Array n s = case unsafeIOToST (js_createUint8Array_wasm n) of ST f -> f s
js_createUint16Array       :: Int -> State# s -> (# State# s,  SomeUint16Array m #)
js_createUint16Array n s = case unsafeIOToST (js_createUint16Array_wasm n) of ST f -> f s
js_createUint32Array       :: Int -> State# s -> (# State# s,  SomeUint32Array m #)
js_createUint32Array n s = case unsafeIOToST (js_createUint32Array_wasm n) of ST f -> f s
js_createFloat32Array      :: Int -> State# s -> (# State# s,  SomeFloat32Array m #)
js_createFloat32Array n s = case unsafeIOToST (js_createFloat32Array_wasm n) of ST f -> f s
js_createFloat64Array      :: Int -> State# s -> (# State# s,  SomeFloat64Array m #)
js_createFloat64Array n s = case unsafeIOToST (js_createFloat64Array_wasm n) of ST f -> f s
#else
foreign import javascript safe
  "((x,y) => { return y[x]; })" js_indexI
  :: Int -> SomeTypedArray e m -> State# s -> (# State# s, Int# #)
foreign import javascript safe
  "((x,y) => { return y[x]; })" js_indexW
  :: Int -> SomeTypedArray e m -> State# s -> (# State# s, Word# #)
foreign import javascript safe
  "((x,y) => { return y[x]; })" js_indexD
  :: Int -> SomeTypedArray e m -> State# s -> (# State# s, Double #)

foreign import javascript unsafe
  "((x,y) => { return y[x]; })" js_unsafeIndexI
  :: Int -> SomeTypedArray e m -> State# s -> (# State# s, Int# #)
foreign import javascript unsafe
  "((x,y) => { return y[x]; })" js_unsafeIndexW
  :: Int -> SomeTypedArray e m -> State# s -> (# State# s, Word# #)
foreign import javascript unsafe
  "((x,y) => { return y[x]; })" js_unsafeIndexD
  :: Int -> SomeTypedArray e m -> State# s -> (# State# s, Double #)

-- -----------------------------------------------------------------------------
-- setIndex

foreign import javascript safe
  "((x,y,z) => { z[x] = y; })" js_setIndexI
  :: Int -> Int# -> SomeTypedArray e m -> State# s -> (# State# s, () #)
foreign import javascript safe
  "((x,y,z) => { z[x] = y; })" js_setIndexW
  :: Int -> Word# -> SomeTypedArray e m -> State# s -> (# State# s, () #)
foreign import javascript safe
  "((x,y,z) => { z[x] = y; })" js_setIndexD
  :: Int -> Double -> SomeTypedArray e m -> State# s -> (# State# s, () #)

foreign import javascript unsafe
  "((x,y,z) => { z[x] = y; })" js_unsafeSetIndexI
  :: Int -> Int# -> SomeTypedArray e m -> State# s -> (# State# s, () #)
foreign import javascript unsafe
  "((x,y,z) => { z[x] = y; })" js_unsafeSetIndexW
  :: Int -> Word# -> SomeTypedArray e m -> State# s -> (# State# s, () #)
foreign import javascript unsafe
  "((x,y,z) => { z[x] = y; })" js_unsafeSetIndexD
  :: Int -> Double -> SomeTypedArray e m -> State# s -> (# State# s, () #)

-- ------------------------------------------------------------------------------

foreign import javascript unsafe
  "$3.indexOf($2,$1)" js_indexOfI
  :: Int -> Int#   -> SomeTypedArray e m -> State# s -> (# State# s, Int #)
foreign import javascript unsafe
  "$3.indexOf($2,$1)" js_indexOfW
  :: Int -> Word#  -> SomeTypedArray e m -> State# s -> (# State# s, Int #)
foreign import javascript unsafe
  "$3.indexOf($2,$1)" js_indexOfD
  :: Int -> Double -> SomeTypedArray e m -> State# s -> (# State# s, Int #)

foreign import javascript unsafe
  "$3.lastIndexOf($2,$1)" js_lastIndexOfI
  :: Int -> Int# -> SomeTypedArray e m -> State# s -> (# State# s, Int #)
foreign import javascript unsafe
  "$3.lastIndexOf($2,$1)" js_lastIndexOfW
  :: Int -> Word# -> SomeTypedArray e m -> State# s -> (# State# s, Int #)
foreign import javascript unsafe
  "$3.lastIndexOf($2,$1)" js_lastIndexOfD
  :: Int -> Double -> SomeTypedArray e m -> State# s -> (# State# s, Int #)

-- ------------------------------------------------------------------------------
-- create

foreign import javascript unsafe
  "((x) => { return new Int8Array(x); })"
  js_createInt8Array         :: Int -> State# s -> (# State# s,  SomeInt8Array m #)
foreign import javascript unsafe
  "((x) => { return new Int16Array(x); })"
  js_createInt16Array        :: Int -> State# s -> (# State# s,  SomeInt16Array m #)
foreign import javascript unsafe
  "((x) => { return new Int32Array(x); })"
  js_createInt32Array        :: Int -> State# s -> (# State# s,  SomeInt32Array m #)

foreign import javascript unsafe
  "((x) => { return new Uint8ClampedArray(x); })"
  js_createUint8ClampedArray :: Int -> State# s -> (# State# s,  SomeUint8ClampedArray m #)
foreign import javascript unsafe
  "((x) => { return new Uint8Array(x); })"
  js_createUint8Array        :: Int -> State# s -> (# State# s,  SomeUint8Array m #)
foreign import javascript unsafe
  "((x) => { return new Uint16Array(x); })"
  js_createUint16Array       :: Int -> State# s -> (# State# s,  SomeUint16Array m #)
foreign import javascript unsafe
  "((x) => { return new Uint32Array(x); })"
  js_createUint32Array       :: Int -> State# s -> (# State# s,  SomeUint32Array m #)

foreign import javascript unsafe
  "((x) => { return new Float32Array(x); })"
  js_createFloat32Array      :: Int -> State# s -> (# State# s,  SomeFloat32Array m #)
foreign import javascript unsafe
  "((x) => { return new Float64Array(x); })"
  js_createFloat64Array      :: Int -> State# s -> (# State# s,  SomeFloat64Array m #)
#endif

-- ------------------------------------------------------------------------------
-- from array

#if defined(wasm32_HOST_ARCH)
foreign import javascript unsafe "((x) => { return Int8Array.from(x); })($1)"
#else
foreign import javascript unsafe
  "((x) => { return Int8Array.from(x); })"
#endif
  js_int8ArrayFromArray         :: SomeJSArray m0 -> IO (SomeInt8Array m1)
#if defined(wasm32_HOST_ARCH)
foreign import javascript unsafe "((x) => { return Int16Array.from(x); })($1)"
#else
foreign import javascript unsafe
  "((x) => { return Int16Array.from(x); })"
#endif
  js_int16ArrayFromArray        :: SomeJSArray m0 -> IO (SomeInt16Array m1)
#if defined(wasm32_HOST_ARCH)
foreign import javascript unsafe "((x) => { return Int32Array.from(x); })($1)"
#else
foreign import javascript unsafe
  "((x) => { return Int32Array.from(x); })"
#endif
  js_int32ArrayFromArray        :: SomeJSArray m0 -> IO (SomeInt32Array m1)
#if defined(wasm32_HOST_ARCH)
foreign import javascript unsafe "((x) => { return Uint8ClampedArray.from(x); })($1)"
#else
foreign import javascript unsafe
  "((x) => { return Uint8ClampedArray.from(x); })"
#endif
  js_uint8ClampedArrayFromArray :: SomeJSArray m0 -> IO (SomeUint8ClampedArray m1)
#if defined(wasm32_HOST_ARCH)
foreign import javascript unsafe "((x) => { return Uint8Array.from(x); })($1)"
#else
foreign import javascript unsafe
  "((x) => { return Uint8Array.from(x); })"
#endif
  js_uint8ArrayFromArray        :: SomeJSArray m0 -> IO (SomeUint8Array m1)
#if defined(wasm32_HOST_ARCH)
foreign import javascript unsafe "((x) => { return Uint16Array.from(x); })($1)"
#else
foreign import javascript unsafe
  "((x) => { return Uint16Array.from(x); })"
#endif
  js_uint16ArrayFromArray       :: SomeJSArray m0 -> IO (SomeUint16Array m1)
#if defined(wasm32_HOST_ARCH)
foreign import javascript unsafe "((x) => { return Uint32Array.from(x); })($1)"
#else
foreign import javascript unsafe
  "((x) => { return Uint32Array.from(x); })"
#endif
  js_uint32ArrayFromArray       :: SomeJSArray m0 -> IO (SomeUint32Array m1)
#if defined(wasm32_HOST_ARCH)
foreign import javascript unsafe "((x) => { return Float32Array.from(x); })($1)"
#else
foreign import javascript unsafe
  "((x) => { return Float32Array.from(x); })"
#endif
  js_float32ArrayFromArray      :: SomeJSArray m0 -> IO (SomeFloat32Array m1)
#if defined(wasm32_HOST_ARCH)
foreign import javascript unsafe "((x) => { return Float64Array.from(x); })($1)"
#else
foreign import javascript unsafe
  "((x) => { return Float64Array.from(x); })"
#endif
  js_float64ArrayFromArray      :: SomeJSArray m0 -> IO (SomeFloat64Array m1)

-- ------------------------------------------------------------------------------
-- from ArrayBuffer

#if defined(wasm32_HOST_ARCH)
foreign import javascript unsafe "((x) => { return new Int8Array(x); })($1)"
#else
foreign import javascript unsafe
  "((x) => { return new Int8Array(x); })"
#endif
  js_int8ArrayFromJSVal         :: JSVal -> SomeInt8Array m
#if defined(wasm32_HOST_ARCH)
foreign import javascript unsafe "((x) => { return new Int16Array(x); })($1)"
#else
foreign import javascript unsafe
  "((x) => { return new Int16Array(x); })"
#endif
  js_int16ArrayFromJSVal        :: JSVal -> SomeInt16Array m
#if defined(wasm32_HOST_ARCH)
foreign import javascript unsafe "((x) => { return new Int32Array(x); })($1)"
#else
foreign import javascript unsafe
  "((x) => { return new Int32Array(x); })"
#endif
  js_int32ArrayFromJSVal        :: JSVal -> SomeInt32Array m
#if defined(wasm32_HOST_ARCH)
foreign import javascript unsafe "((x) => { return new Uint8ClampedArray(x); })($1)"
#else
foreign import javascript unsafe
  "((x) => { return new Uint8ClampedArray(x); })"
#endif
  js_uint8ClampedArrayFromJSVal :: JSVal -> SomeUint8ClampedArray m
#if defined(wasm32_HOST_ARCH)
foreign import javascript unsafe "((x) => { return new Uint8Array(x); })($1)"
#else
foreign import javascript unsafe
  "((x) => { return new Uint8Array(x); })"
#endif
  js_uint8ArrayFromJSVal        :: JSVal -> SomeUint8Array m
#if defined(wasm32_HOST_ARCH)
foreign import javascript unsafe "((x) => { return new Uint16Array(x); })($1)"
#else
foreign import javascript unsafe
  "((x) => { return new Uint16Array(x); })"
#endif
  js_uint16ArrayFromJSVal       :: JSVal -> SomeUint16Array m
#if defined(wasm32_HOST_ARCH)
foreign import javascript unsafe "((x) => { return new Uint32Array(x); })($1)"
#else
foreign import javascript unsafe
  "((x) => { return new Uint32Array(x); })"
#endif
  js_uint32ArrayFromJSVal       :: JSVal -> SomeUint32Array m
#if defined(wasm32_HOST_ARCH)
foreign import javascript unsafe "((x) => { return new Float32Array(x); })($1)"
#else
foreign import javascript unsafe
  "((x) => { return new Float32Array(x); })"
#endif
  js_float32ArrayFromJSVal      :: JSVal -> SomeFloat32Array m
#if defined(wasm32_HOST_ARCH)
foreign import javascript unsafe "((x) => { return new Float64Array(x); })($1)"
#else
foreign import javascript unsafe
  "((x) => { return new Float64Array(x); })"
#endif
  js_float64ArrayFromJSVal      :: JSVal -> SomeFloat64Array m

