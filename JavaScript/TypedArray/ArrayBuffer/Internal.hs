{-# LANGUAGE CPP #-}
{-# LANGUAGE ForeignFunctionInterface #-}
{-# LANGUAGE JavaScriptFFI #-}
{-# LANGUAGE UnliftedFFITypes #-}
{-# LANGUAGE GHCForeignImportPrim #-}
{-# LANGUAGE MagicHash #-}
{-# LANGUAGE UnboxedTuples #-}
{-# LANGUAGE MagicHash #-}
{-# LANGUAGE TypeSynonymInstances #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE PolyKinds #-}
{-# LANGUAGE DeriveDataTypeable #-}

module JavaScript.TypedArray.ArrayBuffer.Internal where

import GHCJS.Types

import GHCJS.Internal.Types
import GHCJS.Marshal.Pure

import GHC.Exts (State#)
#if defined(wasm32_HOST_ARCH)
import GHC.IO (unsafeIOToST)
import GHC.ST (ST(..))
#endif

import Data.Typeable

newtype SomeArrayBuffer (a :: MutabilityType s) =
  SomeArrayBuffer JSVal deriving Typeable
instance IsJSVal (SomeArrayBuffer m)

type ArrayBuffer           = SomeArrayBuffer Immutable
type MutableArrayBuffer    = SomeArrayBuffer Mutable
type STArrayBuffer s       = SomeArrayBuffer (STMutable s)

instance PToJSVal MutableArrayBuffer where
  pToJSVal (SomeArrayBuffer b) = b
instance PFromJSVal MutableArrayBuffer where
  pFromJSVal = SomeArrayBuffer

-- ----------------------------------------------------------------------------

#if defined(wasm32_HOST_ARCH)
foreign import javascript unsafe "((x) => { return x.byteLength; })($1)" js_byteLength :: SomeArrayBuffer any -> Int
#else
foreign import javascript unsafe
  "((x) => { return x.byteLength; })" js_byteLength :: SomeArrayBuffer any -> Int
#endif
#if defined(wasm32_HOST_ARCH)
-- wasm: boxed IO imports plus wrappers with the JavaScript-backend
-- State#-threaded types (used by JavaScript.TypedArray.ArrayBuffer{,.ST}).
foreign import javascript unsafe "new ArrayBuffer($1)"
  js_create_wasm :: Int -> IO JSVal
js_create :: Int -> State# s -> (# State# s, JSVal #)
js_create n s = case unsafeIOToST (js_create_wasm n) of ST f -> f s
foreign import javascript unsafe "$2.slice($1)"
  js_slice1_wasm :: Int -> JSVal -> IO JSVal
js_slice1 :: Int -> JSVal -> State# s -> (# State# s, JSVal #)
js_slice1 n b s = case unsafeIOToST (js_slice1_wasm n b) of ST f -> f s
#else
foreign import javascript unsafe
  "((x) => { return new ArrayBuffer(x); })" js_create :: Int -> State# s -> (# State# s, JSVal #)
foreign import javascript unsafe
  "((x,y) => { return y.slice(x); })" js_slice1 :: Int -> JSVal -> State# s -> (# State# s, JSVal #)
#endif

-- ----------------------------------------------------------------------------
-- immutable non-IO slice

#if defined(wasm32_HOST_ARCH)
foreign import javascript unsafe "((x,y) => { return y.slice(x); })($1,$2)" js_slice1_imm :: Int -> SomeArrayBuffer any -> SomeArrayBuffer any
#else
foreign import javascript unsafe
  "((x,y) => { return y.slice(x); })" js_slice1_imm :: Int -> SomeArrayBuffer any -> SomeArrayBuffer any
#endif
#if defined(wasm32_HOST_ARCH)
foreign import javascript unsafe "((x,y,z) => { return z.slice(x,y); })($1,$2,$3)" js_slice_imm :: Int -> Int -> SomeArrayBuffer any -> SomeArrayBuffer any
#else
foreign import javascript unsafe
  "((x,y,z) => { return z.slice(x,y); })" js_slice_imm :: Int -> Int -> SomeArrayBuffer any -> SomeArrayBuffer any
#endif
