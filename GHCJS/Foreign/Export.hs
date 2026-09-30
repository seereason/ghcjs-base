{-# LANGUAGE CPP #-}
{-# LANGUAGE ForeignFunctionInterface #-}
{-# LANGUAGE JavaScriptFFI #-}
{-# LANGUAGE UnliftedFFITypes #-}
{-# LANGUAGE GHCForeignImportPrim #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE UnboxedTuples #-}
{-# LANGUAGE MagicHash #-}
{-# LANGUAGE EmptyDataDecls #-}

{- | 
     Dynamically export Haskell values to JavaScript
 -}

module GHCJS.Foreign.Export
#if defined(wasm32_HOST_ARCH)
    -- On wasm a foreign import can only take an Export if its
    -- constructor is in scope.
    ( Export(..)
#else
    ( Export
#endif
    , export
    , withExport
    , derefExport
    , releaseExport
    ) where

import Control.Exception (bracket)
import GHC.Exts (Any)
import GHC.Fingerprint
import Data.Typeable
import Data.Word
import Unsafe.Coerce
import qualified GHC.Exts as Exts

import GHC.JS.Prim
import GHCJS.Types
#if defined(wasm32_HOST_ARCH)
import Foreign.Ptr (Ptr, nullPtr)
import Foreign.StablePtr
#endif

newtype Export a = Export JSVal
instance IsJSVal (Export a)

{- |
     Export any Haskell value to a JavaScript reference without evaluating it.
     The JavaScript reference can be passed to foreign code and used to retrieve
     the value later.

     The data referenced by the value will be kept in memory until you call
     'releaseExport', even if no foreign code references the export anymore.
 -}
export :: Typeable a => a -> IO (Export a)
export x = js_export w1 w2 (unsafeCoerce x)
  where
    Fingerprint w1 w2 = typeRepFingerprint (typeOf x)

{- |
     Export the value and run the action. The value is only exported for the
     duration of the action. Dereferencing it after the 'withExport' call
     has returned will always return 'Nothing'.
 -}
-- fixme is this safe with nested exports?
withExport :: Typeable a => a -> (Export a -> IO b) -> IO b
withExport x m = bracket (export x) releaseExport m

{- |
     Retrieve the Haskell value from an export. Returns 'Nothing' if the
     type does not match or the export has already been released.
 -}

derefExport :: forall a. Typeable a => Export a -> IO (Maybe a)
derefExport e = do
  let Fingerprint w1 w2 = typeRepFingerprint (typeOf (undefined :: a))
  r <- js_derefExport w1 w2 e
  if isNull r
    then return Nothing
    else Just . unsafeCoerce <$> js_toHeapObject r

{- |
     Release all memory associated with the export. Subsequent calls to
     'derefExport' will return 'Nothing'
 -}
releaseExport :: Export a -> IO ()
releaseExport e = js_releaseExport e

-- ----------------------------------------------------------------------------

#if defined(wasm32_HOST_ARCH)
-- On wasm the exported value is kept alive by a StablePtr, which is stored
-- (as a number) in the JavaScript export object together with the type
-- fingerprint (two BigInts).  derefExport gets the StablePtr number back
-- as a JSVal (null if released or the fingerprint does not match) and
-- js_toHeapObject dereferences it.
foreign import javascript unsafe
  "({ fp1: $1, fp2: $2, sp: $3, released: false })"
  js_export_wasm :: Word64 -> Word64 -> Ptr () -> IO JSVal
js_export :: Word64 -> Word64 -> Any -> IO (Export a)
js_export w1 w2 x = do
  sp <- newStablePtr x
  Export <$> js_export_wasm w1 w2 (castStablePtrToPtr sp)
foreign import javascript unsafe
  "if(!$3 || typeof $3 !== 'object') return null; if($3.released) return null; if($1 !== $3.fp1 || $2 !== $3.fp2) return null; return $3.sp;"
  js_derefExport :: Word64 -> Word64 -> Export a -> IO JSVal
foreign import javascript unsafe "$1" js_jsvalToPtr :: JSVal -> IO (Ptr ())
js_toHeapObject :: JSVal -> IO Any
js_toHeapObject r = do
  p <- js_jsvalToPtr r
  deRefStablePtr (castPtrToStablePtr p)
-- returns the StablePtr to free, or 0 if the export was already released
foreign import javascript unsafe
  "if($1.released) return 0; var sp = $1.sp; $1.released = true; $1.sp = 0; return sp;"
  js_releaseExport_wasm :: Export a -> IO (Ptr ())
js_releaseExport :: Export a -> IO ()
js_releaseExport e = do
  p <- js_releaseExport_wasm e
  if p == nullPtr
    then return ()
    else freeStablePtr (castPtrToStablePtr p :: StablePtr Any)
#else
foreign import javascript unsafe
  "h$exportValue"
  js_export :: Word64 -> Word64 -> Any -> IO (Export a)
foreign import javascript unsafe
  "h$derefExport"
  js_derefExport :: Word64 -> Word64 -> Export a -> IO JSVal
foreign import javascript unsafe
  "((x) => { return x; })" js_toHeapObject :: JSVal -> IO Any
foreign import javascript unsafe
  "h$releaseExport"
  js_releaseExport :: Export a -> IO ()
#endif
