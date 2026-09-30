{-# LANGUAGE CPP #-}
{-# LANGUAGE ForeignFunctionInterface #-}
{-# LANGUAGE JavaScriptFFI #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE EmptyDataDecls #-}

module JavaScript.Web.Blob.Internal where

import Data.Typeable

import GHCJS.Types
#if defined(wasm32_HOST_ARCH)
-- the wasm JSFFI only unwraps newtypes whose constructors are in scope
import Data.JSString.Internal.Type (JSString(..))
#endif

data BlobType = BlobTypeBlob
              | BlobTypeFile

newtype SomeBlob (a :: BlobType) = SomeBlob JSVal deriving Typeable

type File = SomeBlob BlobTypeFile
type Blob = SomeBlob BlobTypeBlob
  
size :: SomeBlob a -> Int
size b = js_size b
{-# INLINE size #-}

contentType :: SomeBlob a -> JSString
contentType b = js_type b
{-# INLINE contentType #-}

-- is the type correct, does slicing a File give another File?
slice :: Int -> Int -> JSString -> SomeBlob a -> SomeBlob a
slice start end contentType b = js_slice start end contentType b
{-# INLINE slice #-}

isClosed :: SomeBlob a -> IO Bool
isClosed b = js_isClosed b
{-# INLINE isClosed #-}

close :: SomeBlob a -> IO ()
close b = js_close b
{-# INLINE close #-}

-- -----------------------------------------------------------------------------

#if defined(wasm32_HOST_ARCH)
foreign import javascript unsafe "((x) => { return x.size; })($1)" js_size :: SomeBlob a -> Int
#else
foreign import javascript unsafe "((x) => { return x.size; })" js_size :: SomeBlob a -> Int
#endif
#if defined(wasm32_HOST_ARCH)
foreign import javascript unsafe "((x) => { return x.type; })($1)" js_type :: SomeBlob a -> JSString
#else
foreign import javascript unsafe "((x) => { return x.type; })" js_type :: SomeBlob a -> JSString
#endif

-- fixme figure out if we need to support older browsers with obsolete slice
#if defined(wasm32_HOST_ARCH)
foreign import javascript unsafe "(($1,$2,$3,$4) => { return $4.slice($1,$2,$3); })($1,$2,$3,$4)"
#else
foreign import javascript unsafe "(($1,$2,$3,$4) => { return $4.slice($1,$2,$3); })"
#endif
  js_slice :: Int -> Int -> JSString -> SomeBlob a -> SomeBlob a

#if defined(wasm32_HOST_ARCH)
foreign import javascript unsafe "((x) => { return x.isClosed; })($1)"
#else
foreign import javascript unsafe "((x) => { return x.isClosed; })"
#endif
  js_isClosed :: SomeBlob a -> IO Bool
#if defined(wasm32_HOST_ARCH)
foreign import javascript unsafe "((x) => { return x.close(); })($1)"
#else
foreign import javascript unsafe "((x) => { return x.close(); })"
#endif
  js_close :: SomeBlob a -> IO ()
