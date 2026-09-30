{-# LANGUAGE CPP #-}
{-# LANGUAGE JavaScriptFFI #-}
{-# LANGUAGE ForeignFunctionInterface #-}
{-# LANGUAGE UnliftedFFITypes #-}
{-# LANGUAGE GHCForeignImportPrim #-}
{-# LANGUAGE MagicHash #-}
{-# LANGUAGE UnboxedTuples #-}
{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE PolyKinds #-}

module JavaScript.TypedArray.DataView.Internal where

import Data.Int
import Data.Typeable
import Data.Word

import GHC.Exts ( State# )
#if defined(wasm32_HOST_ARCH)
import GHC.IO ( unsafeIOToST )
import GHC.ST ( ST(..) )
#endif

import GHC.JS.Prim
import GHCJS.Internal.Types

import JavaScript.TypedArray.ArrayBuffer.Internal

newtype SomeDataView (a :: MutabilityType s) = SomeDataView JSVal
  deriving Typeable

type DataView        = SomeDataView Immutable
type MutableDataView = SomeDataView Mutable
type STDataView s    = SomeDataView (STMutable s)

#if defined(wasm32_HOST_ARCH)
-- wasm: all of these are synchronous ('unsafe').  The State#-threaded
-- mutable getters/setters are wrappers around boxed IO imports.
-- NB: the JavaScript-backend setters call x.setInt8(x,y) etc. on the
-- *offset* argument, which always throws a TypeError; the wasm versions
-- call the setter on the DataView (the third argument) as intended.

foreign import javascript unsafe "new DataView($1)"
    js_dataView1 :: JSVal -> JSVal
foreign import javascript unsafe "new DataView($2,$1)"
    js_dataView2 :: Int -> JSVal -> SomeDataView m
foreign import javascript unsafe "new DataView($2,$1)"
    js_unsafeDataView2 :: Int -> JSVal-> SomeDataView m
foreign import javascript unsafe "new DataView($3,$1,$2)"
    js_dataView :: Int -> Int -> JSVal -> SomeDataView m
foreign import javascript unsafe "new DataView($3,$1,$2)"
    js_unsafeDataView :: Int -> Int -> JSVal -> JSVal
foreign import javascript unsafe "new DataView($1.buffer.slice($1.byteOffset, $1.byteLength))"
    js_cloneDataView :: SomeDataView m -> IO (SomeDataView m1)

-- ----------------------------------------------------------------------------
-- immutable getters

foreign import javascript unsafe "$2.getInt8($1)" js_i_unsafeGetInt8 :: Int -> DataView -> Int8
foreign import javascript unsafe "$2.getUint8($1)" js_i_unsafeGetUint8 :: Int -> DataView -> Word8
foreign import javascript unsafe "$2.getInt16($1)" js_i_unsafeGetInt16BE :: Int -> DataView -> Int16
foreign import javascript unsafe "$2.getInt32($1)" js_i_unsafeGetInt32BE :: Int -> DataView -> Int
foreign import javascript unsafe "$2.getUint16($1)" js_i_unsafeGetUint16BE :: Int -> DataView -> Word16
foreign import javascript unsafe "$2.getUint32($1)|0" js_i_unsafeGetUint32BE :: Int -> DataView -> Word
foreign import javascript unsafe "$2.getFloat32($1)" js_i_unsafeGetFloat32BE :: Int -> DataView -> Double
foreign import javascript unsafe "$2.getFloat64($1)" js_i_unsafeGetFloat64BE :: Int -> DataView -> Double
foreign import javascript unsafe "$2.getInt16($1,true)" js_i_unsafeGetInt16LE :: Int -> DataView -> Int16
foreign import javascript unsafe "$2.getInt32($1,true)" js_i_unsafeGetInt32LE :: Int -> DataView -> Int
foreign import javascript unsafe "$2.getUint16($1,true)" js_i_unsafeGetUint16LE :: Int -> DataView -> Word16
foreign import javascript unsafe "$2.getUint32($1,true)|0" js_i_unsafeGetUint32LE :: Int -> DataView -> Word
foreign import javascript unsafe "$2.getFloat32($1,true)" js_i_unsafeGetFloat32LE :: Int -> DataView -> Double
foreign import javascript unsafe "$2.getFloat64($1,true)" js_i_unsafeGetFloat64LE :: Int -> DataView -> Double
foreign import javascript unsafe "$2.getInt8($1)" js_i_getInt8 :: Int -> DataView -> Int8
foreign import javascript unsafe "$2.getUint8($1)" js_i_getUint8 :: Int -> DataView -> Word8
foreign import javascript unsafe "$2.getInt16($1)" js_i_getInt16BE :: Int -> DataView -> Int16
foreign import javascript unsafe "$2.getInt32($1)" js_i_getInt32BE :: Int -> DataView -> Int
foreign import javascript unsafe "$2.getUint16($1)" js_i_getUint16BE :: Int -> DataView -> Word16
foreign import javascript unsafe "$2.getUint32($1)|0" js_i_getUint32BE :: Int -> DataView -> Word
foreign import javascript unsafe "$2.getFloat32($1)" js_i_getFloat32BE :: Int -> DataView -> Double
foreign import javascript unsafe "$2.getFloat64($1)" js_i_getFloat64BE :: Int -> DataView -> Double
foreign import javascript unsafe "$2.getInt16($1,true)" js_i_getInt16LE :: Int -> DataView -> Int16
foreign import javascript unsafe "$2.getInt32($1,true)" js_i_getInt32LE :: Int -> DataView -> Int
foreign import javascript unsafe "$2.getUint16($1,true)" js_i_getUint16LE :: Int -> DataView -> Word16
foreign import javascript unsafe "$2.getUint32($1,true)|0" js_i_getUint32LE :: Int -> DataView -> Word
foreign import javascript unsafe "$2.getFloat32($1,true)" js_i_getFloat32LE :: Int -> DataView -> Double
foreign import javascript unsafe "$2.getFloat64($1,true)" js_i_getFloat64LE :: Int -> DataView -> Double

-- ----------------------------------------------------------------------------
-- mutable getters
foreign import javascript unsafe "$2.getInt8($1)" js_m_unsafeGetInt8_wasm :: Int -> SomeDataView m -> IO Int8
js_m_unsafeGetInt8 :: Int -> SomeDataView m -> State# s -> (# State# s, Int8   #)
js_m_unsafeGetInt8 i d s = case unsafeIOToST (js_m_unsafeGetInt8_wasm i d) of ST f -> f s
foreign import javascript unsafe "$2.getUint8($1)" js_m_unsafeGetUint8_wasm :: Int -> SomeDataView m -> IO Word8
js_m_unsafeGetUint8 :: Int -> SomeDataView m -> State# s -> (# State# s, Word8  #)
js_m_unsafeGetUint8 i d s = case unsafeIOToST (js_m_unsafeGetUint8_wasm i d) of ST f -> f s
foreign import javascript unsafe "$2.getInt16($1)" js_m_unsafeGetInt16BE_wasm :: Int -> SomeDataView m -> IO Int16
js_m_unsafeGetInt16BE :: Int -> SomeDataView m -> State# s -> (# State# s, Int16  #)
js_m_unsafeGetInt16BE i d s = case unsafeIOToST (js_m_unsafeGetInt16BE_wasm i d) of ST f -> f s
foreign import javascript unsafe "$2.getInt32($1)" js_m_unsafeGetInt32BE_wasm :: Int -> SomeDataView m -> IO Int
js_m_unsafeGetInt32BE :: Int -> SomeDataView m -> State# s -> (# State# s, Int    #)
js_m_unsafeGetInt32BE i d s = case unsafeIOToST (js_m_unsafeGetInt32BE_wasm i d) of ST f -> f s
foreign import javascript unsafe "$2.getUint16($1)" js_m_unsafeGetUint16BE_wasm :: Int -> SomeDataView m -> IO Word16
js_m_unsafeGetUint16BE :: Int -> SomeDataView m -> State# s -> (# State# s, Word16 #)
js_m_unsafeGetUint16BE i d s = case unsafeIOToST (js_m_unsafeGetUint16BE_wasm i d) of ST f -> f s
foreign import javascript unsafe "$2.getUint32($1)|0" js_m_unsafeGetUint32BE_wasm :: Int -> SomeDataView m -> IO Word
js_m_unsafeGetUint32BE :: Int -> SomeDataView m -> State# s -> (# State# s, Word   #)
js_m_unsafeGetUint32BE i d s = case unsafeIOToST (js_m_unsafeGetUint32BE_wasm i d) of ST f -> f s
foreign import javascript unsafe "$2.getFloat32($1)" js_m_unsafeGetFloat32BE_wasm :: Int -> SomeDataView m -> IO Double
js_m_unsafeGetFloat32BE :: Int -> SomeDataView m -> State# s -> (# State# s, Double #)
js_m_unsafeGetFloat32BE i d s = case unsafeIOToST (js_m_unsafeGetFloat32BE_wasm i d) of ST f -> f s
foreign import javascript unsafe "$2.getFloat64($1)" js_m_unsafeGetFloat64BE_wasm :: Int -> SomeDataView m -> IO Double
js_m_unsafeGetFloat64BE :: Int -> SomeDataView m -> State# s -> (# State# s, Double #)
js_m_unsafeGetFloat64BE i d s = case unsafeIOToST (js_m_unsafeGetFloat64BE_wasm i d) of ST f -> f s
foreign import javascript unsafe "$2.getInt16($1,true)" js_m_unsafeGetInt16LE_wasm :: Int -> SomeDataView m -> IO Int16
js_m_unsafeGetInt16LE :: Int -> SomeDataView m -> State# s -> (# State# s, Int16  #)
js_m_unsafeGetInt16LE i d s = case unsafeIOToST (js_m_unsafeGetInt16LE_wasm i d) of ST f -> f s
foreign import javascript unsafe "$2.getInt32($1,true)" js_m_unsafeGetInt32LE_wasm :: Int -> SomeDataView m -> IO Int
js_m_unsafeGetInt32LE :: Int -> SomeDataView m -> State# s -> (# State# s, Int    #)
js_m_unsafeGetInt32LE i d s = case unsafeIOToST (js_m_unsafeGetInt32LE_wasm i d) of ST f -> f s
foreign import javascript unsafe "$2.getUint16($1,true)" js_m_unsafeGetUint16LE_wasm :: Int -> SomeDataView m -> IO Word16
js_m_unsafeGetUint16LE :: Int -> SomeDataView m -> State# s -> (# State# s, Word16 #)
js_m_unsafeGetUint16LE i d s = case unsafeIOToST (js_m_unsafeGetUint16LE_wasm i d) of ST f -> f s
foreign import javascript unsafe "$2.getUint32($1,true)|0" js_m_unsafeGetUint32LE_wasm :: Int -> SomeDataView m -> IO Word
js_m_unsafeGetUint32LE :: Int -> SomeDataView m -> State# s -> (# State# s, Word   #)
js_m_unsafeGetUint32LE i d s = case unsafeIOToST (js_m_unsafeGetUint32LE_wasm i d) of ST f -> f s
foreign import javascript unsafe "$2.getFloat32($1,true)" js_m_unsafeGetFloat32LE_wasm :: Int -> SomeDataView m -> IO Double
js_m_unsafeGetFloat32LE :: Int -> SomeDataView m -> State# s -> (# State# s, Double #)
js_m_unsafeGetFloat32LE i d s = case unsafeIOToST (js_m_unsafeGetFloat32LE_wasm i d) of ST f -> f s
foreign import javascript unsafe "$2.getFloat64($1,true)" js_m_unsafeGetFloat64LE_wasm :: Int -> SomeDataView m -> IO Double
js_m_unsafeGetFloat64LE :: Int -> SomeDataView m -> State# s -> (# State# s, Double #)
js_m_unsafeGetFloat64LE i d s = case unsafeIOToST (js_m_unsafeGetFloat64LE_wasm i d) of ST f -> f s
foreign import javascript unsafe "$2.getInt8($1)" js_m_getInt8_wasm :: Int -> SomeDataView m -> IO Int8
js_m_getInt8 :: Int -> SomeDataView m -> State# s -> (# State# s, Int8   #)
js_m_getInt8 i d s = case unsafeIOToST (js_m_getInt8_wasm i d) of ST f -> f s
foreign import javascript unsafe "$2.getUint8($1)" js_m_getUint8_wasm :: Int -> SomeDataView m -> IO Word8
js_m_getUint8 :: Int -> SomeDataView m -> State# s -> (# State# s, Word8  #)
js_m_getUint8 i d s = case unsafeIOToST (js_m_getUint8_wasm i d) of ST f -> f s
foreign import javascript unsafe "$2.getInt16($1)" js_m_getInt16BE_wasm :: Int -> SomeDataView m -> IO Int16
js_m_getInt16BE :: Int -> SomeDataView m -> State# s -> (# State# s, Int16  #)
js_m_getInt16BE i d s = case unsafeIOToST (js_m_getInt16BE_wasm i d) of ST f -> f s
foreign import javascript unsafe "$2.getInt32($1)" js_m_getInt32BE_wasm :: Int -> SomeDataView m -> IO Int
js_m_getInt32BE :: Int -> SomeDataView m -> State# s -> (# State# s, Int    #)
js_m_getInt32BE i d s = case unsafeIOToST (js_m_getInt32BE_wasm i d) of ST f -> f s
foreign import javascript unsafe "$2.getUint16($1)" js_m_getUint16BE_wasm :: Int -> SomeDataView m -> IO Word16
js_m_getUint16BE :: Int -> SomeDataView m -> State# s -> (# State# s, Word16 #)
js_m_getUint16BE i d s = case unsafeIOToST (js_m_getUint16BE_wasm i d) of ST f -> f s
foreign import javascript unsafe "$2.getUint32($1)|0" js_m_getUint32BE_wasm :: Int -> SomeDataView m -> IO Word
js_m_getUint32BE :: Int -> SomeDataView m -> State# s -> (# State# s, Word   #)
js_m_getUint32BE i d s = case unsafeIOToST (js_m_getUint32BE_wasm i d) of ST f -> f s
foreign import javascript unsafe "$2.getFloat32($1)" js_m_getFloat32BE_wasm :: Int -> SomeDataView m -> IO Double
js_m_getFloat32BE :: Int -> SomeDataView m -> State# s -> (# State# s, Double #)
js_m_getFloat32BE i d s = case unsafeIOToST (js_m_getFloat32BE_wasm i d) of ST f -> f s
foreign import javascript unsafe "$2.getFloat64($1)" js_m_getFloat64BE_wasm :: Int -> SomeDataView m -> IO Double
js_m_getFloat64BE :: Int -> SomeDataView m -> State# s -> (# State# s, Double #)
js_m_getFloat64BE i d s = case unsafeIOToST (js_m_getFloat64BE_wasm i d) of ST f -> f s
foreign import javascript unsafe "$2.getInt16($1,true)" js_m_getInt16LE_wasm :: Int -> SomeDataView m -> IO Int16
js_m_getInt16LE :: Int -> SomeDataView m -> State# s -> (# State# s, Int16  #)
js_m_getInt16LE i d s = case unsafeIOToST (js_m_getInt16LE_wasm i d) of ST f -> f s
foreign import javascript unsafe "$2.getInt32($1,true)" js_m_getInt32LE_wasm :: Int -> SomeDataView m -> IO Int
js_m_getInt32LE :: Int -> SomeDataView m -> State# s -> (# State# s, Int    #)
js_m_getInt32LE i d s = case unsafeIOToST (js_m_getInt32LE_wasm i d) of ST f -> f s
foreign import javascript unsafe "$2.getUint16($1,true)" js_m_getUint16LE_wasm :: Int -> SomeDataView m -> IO Word16
js_m_getUint16LE :: Int -> SomeDataView m -> State# s -> (# State# s, Word16 #)
js_m_getUint16LE i d s = case unsafeIOToST (js_m_getUint16LE_wasm i d) of ST f -> f s
foreign import javascript unsafe "$2.getUint32($1,true)|0" js_m_getUint32LE_wasm :: Int -> SomeDataView m -> IO Word
js_m_getUint32LE :: Int -> SomeDataView m -> State# s -> (# State# s, Word   #)
js_m_getUint32LE i d s = case unsafeIOToST (js_m_getUint32LE_wasm i d) of ST f -> f s
foreign import javascript unsafe "$2.getFloat32($1,true)" js_m_getFloat32LE_wasm :: Int -> SomeDataView m -> IO Double
js_m_getFloat32LE :: Int -> SomeDataView m -> State# s -> (# State# s, Double #)
js_m_getFloat32LE i d s = case unsafeIOToST (js_m_getFloat32LE_wasm i d) of ST f -> f s
foreign import javascript unsafe "$2.getFloat64($1,true)" js_m_getFloat64LE_wasm :: Int -> SomeDataView m -> IO Double
js_m_getFloat64LE :: Int -> SomeDataView m -> State# s -> (# State# s, Double #)
js_m_getFloat64LE i d s = case unsafeIOToST (js_m_getFloat64LE_wasm i d) of ST f -> f s

-- ----------------------------------------------------------------------------
-- mutable setters
foreign import javascript unsafe "$3.setInt8($1,$2);" js_unsafeSetInt8_wasm :: Int -> Int8 -> SomeDataView m -> IO ()
js_unsafeSetInt8 :: Int -> Int8   -> SomeDataView m -> State# s -> (# State# s, () #)
js_unsafeSetInt8 i x d s = case unsafeIOToST (js_unsafeSetInt8_wasm i x d) of ST f -> f s
foreign import javascript unsafe "$3.setUint8($1,$2);" js_unsafeSetUint8_wasm :: Int -> Word8 -> SomeDataView m -> IO ()
js_unsafeSetUint8 :: Int -> Word8  -> SomeDataView m -> State# s -> (# State# s, () #)
js_unsafeSetUint8 i x d s = case unsafeIOToST (js_unsafeSetUint8_wasm i x d) of ST f -> f s
foreign import javascript unsafe "$3.setInt16($1,$2);" js_unsafeSetInt16BE_wasm :: Int -> Int16 -> SomeDataView m -> IO ()
js_unsafeSetInt16BE :: Int -> Int16  -> SomeDataView m -> State# s -> (# State# s, () #)
js_unsafeSetInt16BE i x d s = case unsafeIOToST (js_unsafeSetInt16BE_wasm i x d) of ST f -> f s
foreign import javascript unsafe "$3.setInt32($1,$2);" js_unsafeSetInt32BE_wasm :: Int -> Int -> SomeDataView m -> IO ()
js_unsafeSetInt32BE :: Int -> Int    -> SomeDataView m -> State# s -> (# State# s, () #)
js_unsafeSetInt32BE i x d s = case unsafeIOToST (js_unsafeSetInt32BE_wasm i x d) of ST f -> f s
foreign import javascript unsafe "$3.setUint16($1,$2);" js_unsafeSetUint16BE_wasm :: Int -> Word16 -> SomeDataView m -> IO ()
js_unsafeSetUint16BE :: Int -> Word16 -> SomeDataView m -> State# s -> (# State# s, () #)
js_unsafeSetUint16BE i x d s = case unsafeIOToST (js_unsafeSetUint16BE_wasm i x d) of ST f -> f s
foreign import javascript unsafe "$3.setUint32($1,$2);" js_unsafeSetUint32BE_wasm :: Int -> Word -> SomeDataView m -> IO ()
js_unsafeSetUint32BE :: Int -> Word   -> SomeDataView m -> State# s -> (# State# s, () #)
js_unsafeSetUint32BE i x d s = case unsafeIOToST (js_unsafeSetUint32BE_wasm i x d) of ST f -> f s
foreign import javascript unsafe "$3.setFloat32($1,$2);" js_unsafeSetFloat32BE_wasm :: Int -> Double -> SomeDataView m -> IO ()
js_unsafeSetFloat32BE :: Int -> Double -> SomeDataView m -> State# s -> (# State# s, () #)
js_unsafeSetFloat32BE i x d s = case unsafeIOToST (js_unsafeSetFloat32BE_wasm i x d) of ST f -> f s
foreign import javascript unsafe "$3.setFloat64($1,$2);" js_unsafeSetFloat64BE_wasm :: Int -> Double -> SomeDataView m -> IO ()
js_unsafeSetFloat64BE :: Int -> Double -> SomeDataView m -> State# s -> (# State# s, () #)
js_unsafeSetFloat64BE i x d s = case unsafeIOToST (js_unsafeSetFloat64BE_wasm i x d) of ST f -> f s
foreign import javascript unsafe "$3.setInt16($1,$2,true);" js_unsafeSetInt16LE_wasm :: Int -> Int16 -> SomeDataView m -> IO ()
js_unsafeSetInt16LE :: Int -> Int16  -> SomeDataView m -> State# s -> (# State# s, () #)
js_unsafeSetInt16LE i x d s = case unsafeIOToST (js_unsafeSetInt16LE_wasm i x d) of ST f -> f s
foreign import javascript unsafe "$3.setInt32($1,$2,true);" js_unsafeSetInt32LE_wasm :: Int -> Int -> SomeDataView m -> IO ()
js_unsafeSetInt32LE :: Int -> Int    -> SomeDataView m -> State# s -> (# State# s, () #)
js_unsafeSetInt32LE i x d s = case unsafeIOToST (js_unsafeSetInt32LE_wasm i x d) of ST f -> f s
foreign import javascript unsafe "$3.setUint16($1,$2,true);" js_unsafeSetUint16LE_wasm :: Int -> Word16 -> SomeDataView m -> IO ()
js_unsafeSetUint16LE :: Int -> Word16 -> SomeDataView m -> State# s -> (# State# s, () #)
js_unsafeSetUint16LE i x d s = case unsafeIOToST (js_unsafeSetUint16LE_wasm i x d) of ST f -> f s
foreign import javascript unsafe "$3.setUint32($1,$2,true);" js_unsafeSetUint32LE_wasm :: Int -> Word -> SomeDataView m -> IO ()
js_unsafeSetUint32LE :: Int -> Word   -> SomeDataView m -> State# s -> (# State# s, () #)
js_unsafeSetUint32LE i x d s = case unsafeIOToST (js_unsafeSetUint32LE_wasm i x d) of ST f -> f s
foreign import javascript unsafe "$3.setFloat32($1,$2,true);" js_unsafeSetFloat32LE_wasm :: Int -> Double -> SomeDataView m -> IO ()
js_unsafeSetFloat32LE :: Int -> Double -> SomeDataView m -> State# s -> (# State# s, () #)
js_unsafeSetFloat32LE i x d s = case unsafeIOToST (js_unsafeSetFloat32LE_wasm i x d) of ST f -> f s
foreign import javascript unsafe "$3.setFloat64($1,$2,true);" js_unsafeSetFloat64LE_wasm :: Int -> Double -> SomeDataView m -> IO ()
js_unsafeSetFloat64LE :: Int -> Double -> SomeDataView m -> State# s -> (# State# s, () #)
js_unsafeSetFloat64LE i x d s = case unsafeIOToST (js_unsafeSetFloat64LE_wasm i x d) of ST f -> f s
foreign import javascript unsafe "$3.setInt8($1,$2);" js_setInt8_wasm :: Int -> Int8 -> SomeDataView m -> IO ()
js_setInt8 :: Int -> Int8   -> SomeDataView m -> State# s -> (# State# s, () #)
js_setInt8 i x d s = case unsafeIOToST (js_setInt8_wasm i x d) of ST f -> f s
foreign import javascript unsafe "$3.setUint8($1,$2);" js_setUint8_wasm :: Int -> Word8 -> SomeDataView m -> IO ()
js_setUint8 :: Int -> Word8  -> SomeDataView m -> State# s -> (# State# s, () #)
js_setUint8 i x d s = case unsafeIOToST (js_setUint8_wasm i x d) of ST f -> f s
foreign import javascript unsafe "$3.setInt16($1,$2);" js_setInt16BE_wasm :: Int -> Int16 -> SomeDataView m -> IO ()
js_setInt16BE :: Int -> Int16  -> SomeDataView m -> State# s -> (# State# s, () #)
js_setInt16BE i x d s = case unsafeIOToST (js_setInt16BE_wasm i x d) of ST f -> f s
foreign import javascript unsafe "$3.setInt32($1,$2);" js_setInt32BE_wasm :: Int -> Int -> SomeDataView m -> IO ()
js_setInt32BE :: Int -> Int    -> SomeDataView m -> State# s -> (# State# s, () #)
js_setInt32BE i x d s = case unsafeIOToST (js_setInt32BE_wasm i x d) of ST f -> f s
foreign import javascript unsafe "$3.setUint16($1,$2);" js_setUint16BE_wasm :: Int -> Word16 -> SomeDataView m -> IO ()
js_setUint16BE :: Int -> Word16 -> SomeDataView m -> State# s -> (# State# s, () #)
js_setUint16BE i x d s = case unsafeIOToST (js_setUint16BE_wasm i x d) of ST f -> f s
foreign import javascript unsafe "$3.setUint32($1,$2);" js_setUint32BE_wasm :: Int -> Word -> SomeDataView m -> IO ()
js_setUint32BE :: Int -> Word   -> SomeDataView m -> State# s -> (# State# s, () #)
js_setUint32BE i x d s = case unsafeIOToST (js_setUint32BE_wasm i x d) of ST f -> f s
foreign import javascript unsafe "$3.setFloat32($1,$2);" js_setFloat32BE_wasm :: Int -> Double -> SomeDataView m -> IO ()
js_setFloat32BE :: Int -> Double -> SomeDataView m -> State# s -> (# State# s, () #)
js_setFloat32BE i x d s = case unsafeIOToST (js_setFloat32BE_wasm i x d) of ST f -> f s
foreign import javascript unsafe "$3.setFloat64($1,$2);" js_setFloat64BE_wasm :: Int -> Double -> SomeDataView m -> IO ()
js_setFloat64BE :: Int -> Double -> SomeDataView m -> State# s -> (# State# s, () #)
js_setFloat64BE i x d s = case unsafeIOToST (js_setFloat64BE_wasm i x d) of ST f -> f s
foreign import javascript unsafe "$3.setInt16($1,$2,true);" js_setInt16LE_wasm :: Int -> Int16 -> SomeDataView m -> IO ()
js_setInt16LE :: Int -> Int16  -> SomeDataView m -> State# s -> (# State# s, () #)
js_setInt16LE i x d s = case unsafeIOToST (js_setInt16LE_wasm i x d) of ST f -> f s
foreign import javascript unsafe "$3.setInt32($1,$2,true);" js_setInt32LE_wasm :: Int -> Int -> SomeDataView m -> IO ()
js_setInt32LE :: Int -> Int    -> SomeDataView m -> State# s -> (# State# s, () #)
js_setInt32LE i x d s = case unsafeIOToST (js_setInt32LE_wasm i x d) of ST f -> f s
foreign import javascript unsafe "$3.setUint16($1,$2,true);" js_setUint16LE_wasm :: Int -> Word16 -> SomeDataView m -> IO ()
js_setUint16LE :: Int -> Word16 -> SomeDataView m -> State# s -> (# State# s, () #)
js_setUint16LE i x d s = case unsafeIOToST (js_setUint16LE_wasm i x d) of ST f -> f s
foreign import javascript unsafe "$3.setUint32($1,$2,true);" js_setUint32LE_wasm :: Int -> Word -> SomeDataView m -> IO ()
js_setUint32LE :: Int -> Word   -> SomeDataView m -> State# s -> (# State# s, () #)
js_setUint32LE i x d s = case unsafeIOToST (js_setUint32LE_wasm i x d) of ST f -> f s
foreign import javascript unsafe "$3.setFloat32($1,$2,true);" js_setFloat32LE_wasm :: Int -> Double -> SomeDataView m -> IO ()
js_setFloat32LE :: Int -> Double -> SomeDataView m -> State# s -> (# State# s, () #)
js_setFloat32LE i x d s = case unsafeIOToST (js_setFloat32LE_wasm i x d) of ST f -> f s
foreign import javascript unsafe "$3.setFloat64($1,$2,true);" js_setFloat64LE_wasm :: Int -> Double -> SomeDataView m -> IO ()
js_setFloat64LE :: Int -> Double -> SomeDataView m -> State# s -> (# State# s, () #)
js_setFloat64LE i x d s = case unsafeIOToST (js_setFloat64LE_wasm i x d) of ST f -> f s
#else
#define JSU foreign import javascript unsafe
#define JSS foreign import javascript safe

JSU "((x) => { return new DataView(x); })"
    js_dataView1 :: JSVal -> JSVal
JSS "((x,y) => { return new DataView(y,x); })"
    js_dataView2 :: Int -> JSVal -> SomeDataView m
JSU "((x,y) => { return new DataView(y,x); })"
    js_unsafeDataView2 :: Int -> JSVal-> SomeDataView m
JSS "((x,y,z) => { return new DataView(z,x,y); })"
    js_dataView :: Int -> Int -> JSVal -> SomeDataView m
JSU "((x,y,z) => { return new DataView(z,x,y); })"
    js_unsafeDataView :: Int -> Int -> JSVal -> JSVal
JSU "((x) => { return new DataView(x.buffer.slice(x.byteOffset, x.byteLength)); })"
    js_cloneDataView :: SomeDataView m -> IO (SomeDataView m1)

-- ----------------------------------------------------------------------------
-- immutable getters

JSU "((x,y) => { return y.getInt8(x); })"          js_i_unsafeGetInt8       :: Int -> DataView -> Int8
JSU "((x,y) => { return y.getUint8(x); })"         js_i_unsafeGetUint8      :: Int -> DataView -> Word8
JSU "((x,y) => { return y.getInt16(x); })"         js_i_unsafeGetInt16BE    :: Int -> DataView -> Int16
JSU "((x,y) => { return y.getInt32(x); })"         js_i_unsafeGetInt32BE    :: Int -> DataView -> Int
JSU "((x,y) => { return y.getUint16(x); })"        js_i_unsafeGetUint16BE   :: Int -> DataView -> Word16
JSU "((x,y) => { return y.getUint32(x)|0; })"      js_i_unsafeGetUint32BE   :: Int -> DataView -> Word
JSU "((x,y) => { return y.getFloat32(x); })"       js_i_unsafeGetFloat32BE  :: Int -> DataView -> Double
JSU "((x,y) => { return y.getFloat64(x); })"       js_i_unsafeGetFloat64BE  :: Int -> DataView -> Double
JSU "((x,y) => { return y.getInt16(x,true); })"    js_i_unsafeGetInt16LE    :: Int -> DataView -> Int16
JSU "((x,y) => { return y.getInt32(x,true); })"    js_i_unsafeGetInt32LE    :: Int -> DataView -> Int
JSU "((x,y) => { return y.getUint16(x,true); })"   js_i_unsafeGetUint16LE   :: Int -> DataView -> Word16
JSU "((x,y) => { return y.getUint32(x,true)|0; })" js_i_unsafeGetUint32LE   :: Int -> DataView -> Word
JSU "((x,y) => { return y.getFloat32(x,true); })"  js_i_unsafeGetFloat32LE  :: Int -> DataView -> Double
JSU "((x,y) => { return y.getFloat64(x,true); })"  js_i_unsafeGetFloat64LE  :: Int -> DataView -> Double

JSS "((x,y) => { return y.getInt8(x); })"          js_i_getInt8       :: Int -> DataView -> Int8
JSS "((x,y) => { return y.getUint8(x); })"         js_i_getUint8      :: Int -> DataView -> Word8
JSS "((x,y) => { return y.getInt16(x); })"         js_i_getInt16BE    :: Int -> DataView -> Int16
JSS "((x,y) => { return y.getInt32(x); })"         js_i_getInt32BE    :: Int -> DataView -> Int
JSS "((x,y) => { return y.getUint16(x); })"        js_i_getUint16BE   :: Int -> DataView -> Word16
JSS "((x,y) => { return y.getUint32(x)|0; })"      js_i_getUint32BE   :: Int -> DataView -> Word
JSS "((x,y) => { return y.getFloat32(x); })"       js_i_getFloat32BE  :: Int -> DataView -> Double
JSS "((x,y) => { return y.getFloat64(x); })"       js_i_getFloat64BE  :: Int -> DataView -> Double
JSS "((x,y) => { return y.getInt16(x,true); })"    js_i_getInt16LE    :: Int -> DataView -> Int16
JSS "((x,y) => { return y.getInt32(x,true); })"    js_i_getInt32LE    :: Int -> DataView -> Int
JSS "((x,y) => { return y.getUint16(x,true); })"   js_i_getUint16LE   :: Int -> DataView -> Word16
JSS "((x,y) => { return y.getUint32(x,true)|0; })" js_i_getUint32LE   :: Int -> DataView -> Word
JSS "((x,y) => { return y.getFloat32(x,true); })"  js_i_getFloat32LE  :: Int -> DataView -> Double
JSS "((x,y) => { return y.getFloat64(x,true); })"  js_i_getFloat64LE  :: Int -> DataView -> Double

-- ----------------------------------------------------------------------------
-- mutable getters

JSU "((x,y) => { return y.getInt8(x); })"          js_m_unsafeGetInt8      :: Int -> SomeDataView m -> State# s -> (# State# s, Int8   #)
JSU "((x,y) => { return y.getUint8(x); })"         js_m_unsafeGetUint8     :: Int -> SomeDataView m -> State# s -> (# State# s, Word8  #)
JSU "((x,y) => { return y.getInt16(x); })"         js_m_unsafeGetInt16BE   :: Int -> SomeDataView m -> State# s -> (# State# s, Int16  #)
JSU "((x,y) => { return y.getInt32(x); })"         js_m_unsafeGetInt32BE   :: Int -> SomeDataView m -> State# s -> (# State# s, Int    #)
JSU "((x,y) => { return y.getUint16(x); })"        js_m_unsafeGetUint16BE  :: Int -> SomeDataView m -> State# s -> (# State# s, Word16 #)
JSU "((x,y) => { return y.getUint32(x)|0; })"      js_m_unsafeGetUint32BE  :: Int -> SomeDataView m -> State# s -> (# State# s, Word   #)
JSU "((x,y) => { return y.getFloat32(x); })"       js_m_unsafeGetFloat32BE :: Int -> SomeDataView m -> State# s -> (# State# s, Double #)
JSU "((x,y) => { return y.getFloat64(x); })"       js_m_unsafeGetFloat64BE :: Int -> SomeDataView m -> State# s -> (# State# s, Double #)
JSU "((x,y) => { return y.getInt16(x,true); })"    js_m_unsafeGetInt16LE   :: Int -> SomeDataView m -> State# s -> (# State# s, Int16  #)
JSU "((x,y) => { return y.getInt32(x,true); })"    js_m_unsafeGetInt32LE   :: Int -> SomeDataView m -> State# s -> (# State# s, Int    #)
JSU "((x,y) => { return y.getUint16(x,true); })"   js_m_unsafeGetUint16LE  :: Int -> SomeDataView m -> State# s -> (# State# s, Word16 #)
JSU "((x,y) => { return y.getUint32(x,true)|0; })" js_m_unsafeGetUint32LE  :: Int -> SomeDataView m -> State# s -> (# State# s, Word   #)
JSU "((x,y) => { return y.getFloat32(x,true); })"  js_m_unsafeGetFloat32LE :: Int -> SomeDataView m -> State# s -> (# State# s, Double #)
JSU "((x,y) => { return y.getFloat64(x,true); })"  js_m_unsafeGetFloat64LE :: Int -> SomeDataView m -> State# s -> (# State# s, Double #)

JSS "((x,y) => { return y.getInt8(x); })"          js_m_getInt8            :: Int -> SomeDataView m -> State# s -> (# State# s, Int8   #)
JSS "((x,y) => { return y.getUint8(x); })"         js_m_getUint8           :: Int -> SomeDataView m -> State# s -> (# State# s, Word8  #)
JSS "((x,y) => { return y.getInt16(x); })"         js_m_getInt16BE         :: Int -> SomeDataView m -> State# s -> (# State# s, Int16  #)
JSS "((x,y) => { return y.getInt32(x); })"         js_m_getInt32BE         :: Int -> SomeDataView m -> State# s -> (# State# s, Int    #)
JSS "((x,y) => { return y.getUint16(x); })"        js_m_getUint16BE        :: Int -> SomeDataView m -> State# s -> (# State# s, Word16 #)
JSS "((x,y) => { return y.getUint32(x)|0; })"      js_m_getUint32BE        :: Int -> SomeDataView m -> State# s -> (# State# s, Word   #)
JSS "((x,y) => { return y.getFloat32(x); })"       js_m_getFloat32BE       :: Int -> SomeDataView m -> State# s -> (# State# s, Double #)
JSS "((x,y) => { return y.getFloat64(x); })"       js_m_getFloat64BE       :: Int -> SomeDataView m -> State# s -> (# State# s, Double #)
JSS "((x,y) => { return y.getInt16(x,true); })"    js_m_getInt16LE         :: Int -> SomeDataView m -> State# s -> (# State# s, Int16  #)
JSS "((x,y) => { return y.getInt32(x,true); })"    js_m_getInt32LE         :: Int -> SomeDataView m -> State# s -> (# State# s, Int    #)
JSS "((x,y) => { return y.getUint16(x,true); })"   js_m_getUint16LE        :: Int -> SomeDataView m -> State# s -> (# State# s, Word16 #)
JSS "((x,y) => { return y.getUint32(x,true)|0; })" js_m_getUint32LE        :: Int -> SomeDataView m -> State# s -> (# State# s, Word   #)
JSS "((x,y) => { return y.getFloat32(x,true); })"  js_m_getFloat32LE       :: Int -> SomeDataView m -> State# s -> (# State# s, Double #)
JSS "((x,y) => { return y.getFloat64(x,true); })"  js_m_getFloat64LE       :: Int -> SomeDataView m -> State# s -> (# State# s, Double #)

-- ----------------------------------------------------------------------------
-- mutable setters

JSU "((x,y,z) => { x.setInt8(x,y); })"         js_unsafeSetInt8      :: Int -> Int8   -> SomeDataView m -> State# s -> (# State# s, () #)
JSU "((x,y,z) => { x.setUint8(x,y); })"        js_unsafeSetUint8     :: Int -> Word8  -> SomeDataView m -> State# s -> (# State# s, () #)
JSU "((x,y,z) => { x.setInt16(x,y); })"        js_unsafeSetInt16BE   :: Int -> Int16  -> SomeDataView m -> State# s -> (# State# s, () #)
JSU "((x,y,z) => { x.setInt32(x,y); })"        js_unsafeSetInt32BE   :: Int -> Int    -> SomeDataView m -> State# s -> (# State# s, () #)
JSU "((x,y,z) => { x.setUint16(x,y); })"       js_unsafeSetUint16BE  :: Int -> Word16 -> SomeDataView m -> State# s -> (# State# s, () #)
JSU "((x,y,z) => { x.setUint32(x,y); })"       js_unsafeSetUint32BE  :: Int -> Word   -> SomeDataView m -> State# s -> (# State# s, () #)
JSU "((x,y,z) => { x.setFloat32(x,y); })"      js_unsafeSetFloat32BE :: Int -> Double -> SomeDataView m -> State# s -> (# State# s, () #)
JSU "((x,y,z) => { x.setFloat64(x,y); })"      js_unsafeSetFloat64BE :: Int -> Double -> SomeDataView m -> State# s -> (# State# s, () #)
JSU "((x,y,z) => { x.setInt16(x,y,true); })"   js_unsafeSetInt16LE   :: Int -> Int16  -> SomeDataView m -> State# s -> (# State# s, () #)
JSU "((x,y,z) => { x.setInt32(x,y,true); })"   js_unsafeSetInt32LE   :: Int -> Int    -> SomeDataView m -> State# s -> (# State# s, () #)
JSU "((x,y,z) => { x.setUint16(x,y,true); })"  js_unsafeSetUint16LE  :: Int -> Word16 -> SomeDataView m -> State# s -> (# State# s, () #)
JSU "((x,y,z) => { x.setUint32(x,y,true); })"  js_unsafeSetUint32LE  :: Int -> Word   -> SomeDataView m -> State# s -> (# State# s, () #)
JSU "((x,y,z) => { x.setFloat32(x,y,true); })" js_unsafeSetFloat32LE :: Int -> Double -> SomeDataView m -> State# s -> (# State# s, () #)
JSU "((x,y,z) => { x.setFloat64(x,y,true); })" js_unsafeSetFloat64LE :: Int -> Double -> SomeDataView m -> State# s -> (# State# s, () #)

JSS "((x,y,z) => { x.setInt8(x,y); })"         js_setInt8            :: Int -> Int8   -> SomeDataView m -> State# s -> (# State# s, () #)
JSS "((x,y,z) => { x.setUint8(x,y); })"        js_setUint8           :: Int -> Word8  -> SomeDataView m -> State# s -> (# State# s, () #)
JSS "((x,y,z) => { x.setInt16(x,y); })"        js_setInt16BE         :: Int -> Int16  -> SomeDataView m -> State# s -> (# State# s, () #)
JSS "((x,y,z) => { x.setInt32(x,y); })"        js_setInt32BE         :: Int -> Int    -> SomeDataView m -> State# s -> (# State# s, () #)
JSS "((x,y,z) => { x.setUint16(x,y); })"       js_setUint16BE        :: Int -> Word16 -> SomeDataView m -> State# s -> (# State# s, () #)
JSS "((x,y,z) => { x.setUint32(x,y); })"       js_setUint32BE        :: Int -> Word   -> SomeDataView m -> State# s -> (# State# s, () #)
JSS "((x,y,z) => { x.setFloat32(x,y); })"      js_setFloat32BE       :: Int -> Double -> SomeDataView m -> State# s -> (# State# s, () #)
JSS "((x,y,z) => { x.setFloat64(x,y); })"      js_setFloat64BE       :: Int -> Double -> SomeDataView m -> State# s -> (# State# s, () #)
JSS "((x,y,z) => { x.setInt16(x,y,true); })"   js_setInt16LE         :: Int -> Int16  -> SomeDataView m -> State# s -> (# State# s, () #)
JSS "((x,y,z) => { x.setInt32(x,y,true); })"   js_setInt32LE         :: Int -> Int    -> SomeDataView m -> State# s -> (# State# s, () #)
JSS "((x,y,z) => { x.setUint16(x,y,true); })"  js_setUint16LE        :: Int -> Word16 -> SomeDataView m -> State# s -> (# State# s, () #)
JSS "((x,y,z) => { x.setUint32(x,y,true); })"  js_setUint32LE        :: Int -> Word   -> SomeDataView m -> State# s -> (# State# s, () #)
JSS "((x,y,z) => { x.setFloat32(x,y,true); })" js_setFloat32LE       :: Int -> Double -> SomeDataView m -> State# s -> (# State# s, () #)
JSS "((x,y,z) => { x.setFloat64(x,y,true); })" js_setFloat64LE       :: Int -> Double -> SomeDataView m -> State# s -> (# State# s, () #)

#endif
