{-# LANGUAGE CPP #-}
{-# LANGUAGE ForeignFunctionInterface, JavaScriptFFI, DataKinds, KindSignatures,
             PolyKinds, UnboxedTuples, GHCForeignImportPrim, DeriveDataTypeable,
             UnliftedFFITypes, MagicHash
  #-}
module JavaScript.Array.Internal where

import           Prelude hiding (length, reverse, drop, take)

import           Control.DeepSeq
import           Data.Typeable
import           Unsafe.Coerce (unsafeCoerce)

import           GHC.Types
import           GHC.IO
import qualified GHC.Exts as Exts
import           GHC.Exts (State#)
#if defined(wasm32_HOST_ARCH)
import           GHC.ST (ST(..))
#endif

import           GHCJS.Internal.Types
import qualified GHC.JS.Prim as Prim
import           GHCJS.Types

newtype SomeJSArray (m :: MutabilityType s) = SomeJSArray JSVal
  deriving (Typeable)
instance IsJSVal (SomeJSArray m)

type JSArray        = SomeJSArray Immutable
type MutableJSArray = SomeJSArray Mutable

type STJSArray s    = SomeJSArray (STMutable s)

create :: IO MutableJSArray
create = IO js_create
{-# INLINE create #-}

length :: JSArray -> Int
length x = js_lengthPure x
{-# INLINE length #-}

lengthIO :: SomeJSArray m -> IO Int
lengthIO x = IO (js_length x)
{-# INLINE lengthIO #-}

null :: JSArray -> Bool
null x = length x == 0
{-# INLINE null #-}

append :: SomeJSArray m -> SomeJSArray m -> IO (SomeJSArray m1)
append x y = IO (js_append x y)
{-# INLINE append #-}

fromList :: [JSVal] -> JSArray
fromList xs = rnf xs `seq` js_toJSArrayPure (unsafeCoerce xs)
{-# INLINE fromList #-}

fromListIO :: [JSVal] -> IO (SomeJSArray m)
fromListIO xs = IO (\s -> rnf xs `seq` js_toJSArray (unsafeCoerce xs) s)
{-# INLINE fromListIO #-}

toList :: JSArray -> [JSVal]
toList x = unsafeCoerce (js_fromJSArrayPure x)
{-# INLINE toList #-}

toListIO :: SomeJSArray m -> IO [JSVal]
toListIO x = IO $ \s -> case js_fromJSArray x s of
                          (# s', xs #) -> (# s', unsafeCoerce xs #)
{-# INLINE toListIO #-}

index :: Int -> JSArray -> JSVal
index n x = js_indexPure n x
{-# INLINE index #-}

read :: Int -> SomeJSArray m -> IO JSVal
read n x = IO (js_index n x)
{-# INLINE read #-}

write :: Int -> JSVal -> MutableJSArray -> IO ()
write n e x = IO (js_setIndex n e x)
{-# INLINE write #-}

push :: JSVal -> MutableJSArray -> IO ()
push e x = IO (js_push e x)
{-# INLINE push #-}

pop :: MutableJSArray -> IO JSVal
pop x = IO (js_pop x)
{-# INLINE pop #-}

unshift :: JSVal -> MutableJSArray -> IO ()
unshift e x = IO (js_unshift e x)
{-# INLINE unshift #-}

shift :: MutableJSArray -> IO JSVal
shift x = IO (js_shift x)
{-# INLINE shift #-}

reverse :: MutableJSArray -> IO ()
reverse x = IO (js_reverse x)
{-# INLINE reverse #-}

take :: Int -> JSArray -> JSArray
take n x = js_slicePure 0 n x
{-# INLINE take #-}

takeIO :: Int -> SomeJSArray m -> IO (SomeJSArray m1)
takeIO n x = IO (js_slice 0 n x)
{-# INLINE takeIO #-}

drop :: Int -> JSArray -> JSArray
drop n x = js_slice1Pure n x
{-# INLINE drop #-}

dropIO :: Int -> SomeJSArray m -> IO (SomeJSArray m1)
dropIO n x = IO (js_slice1 n x)
{-# INLINE dropIO #-}

sliceIO :: Int -> Int -> JSArray -> IO (SomeJSArray m1)
sliceIO s n x = IO (js_slice s n x)
{-# INLINE sliceIO #-}

slice :: Int -> Int -> JSArray -> JSArray
slice s n x = js_slicePure s n x
{-# INLINE slice #-}

freeze :: MutableJSArray -> IO JSArray
freeze x = IO (js_slice1 0 x)
{-# INLINE freeze #-}

unsafeFreeze :: MutableJSArray -> IO JSArray
unsafeFreeze (SomeJSArray x) = pure (SomeJSArray x)
{-# INLINE unsafeFreeze #-}

thaw :: JSArray -> IO MutableJSArray
thaw x = IO (js_slice1 0 x)
{-# INLINE thaw #-}

unsafeThaw :: JSArray -> IO MutableJSArray
unsafeThaw (SomeJSArray x) = pure (SomeJSArray x)
{-# INLINE unsafeThaw #-}


-- -----------------------------------------------------------------------------

#if defined(wasm32_HOST_ARCH)
-- The wasm JSFFI cannot marshal State#-threaded unboxed tuples, so the
-- js_* names below are wrappers (with the JavaScript-backend types, so
-- that JavaScript.Array.ST etc. are unchanged) around boxed IO imports.

foreign import javascript unsafe "[]"
  js_create_wasm :: IO (SomeJSArray m)
js_create   :: State# s -> (# State# s, SomeJSArray m #)
js_create s = case unsafeIOToST js_create_wasm of ST f -> f s

foreign import javascript unsafe "$1.length"
  js_length_wasm :: SomeJSArray m -> IO Int
js_length     :: SomeJSArray m -> State# s -> (# State# s, Int #)
js_length x s = case unsafeIOToST (js_length_wasm x) of ST f -> f s
foreign import javascript unsafe "$2[$1]"
  js_index_wasm :: Int -> SomeJSArray m -> IO JSVal
js_index     :: Int -> SomeJSArray m -> State# s -> (# State# s, JSVal #)
js_index n x s = case unsafeIOToST (js_index_wasm n x) of ST f -> f s
#else
foreign import javascript unsafe "((x) => { return []; })"
  js_create   :: State# s -> (# State# s, SomeJSArray m #)

foreign import javascript unsafe "((x) => { return x.length; })"
  js_length     :: SomeJSArray m -> State# s -> (# State# s, Int #)
foreign import javascript unsafe "((x,y) => { return y[x]; })"
  js_index     :: Int -> SomeJSArray m -> State# s -> (# State# s, JSVal #)
#endif

#if defined(wasm32_HOST_ARCH)
foreign import javascript unsafe "((x,y) => { return y[x]; })($1,$2)"
#else
foreign import javascript unsafe "((x,y) => { return y[x]; })"
#endif
  js_indexPure :: Int -> JSArray -> JSVal
#if defined(wasm32_HOST_ARCH)
foreign import javascript unsafe "((x) => { return x.length; })($1)"
#else
foreign import javascript unsafe "((x) => { return x.length; })"
#endif
  js_lengthPure :: JSArray -> Int

#if defined(wasm32_HOST_ARCH)
foreign import javascript unsafe "$3[$1] = $2;"
  js_setIndex_wasm :: Int -> JSVal -> SomeJSArray m -> IO ()
js_setIndex :: Int -> JSVal -> SomeJSArray m -> State# s -> (# State# s, () #)
js_setIndex n e x s = case unsafeIOToST (js_setIndex_wasm n e x) of ST f -> f s

foreign import javascript unsafe "$3.slice($1,$2)"
  js_slice_wasm :: Int -> Int -> SomeJSArray m -> IO (SomeJSArray m1)
js_slice     :: Int -> Int -> SomeJSArray m -> State# s -> (# State# s, SomeJSArray m1 #)
js_slice b e x s = case unsafeIOToST (js_slice_wasm b e x) of ST f -> f s
foreign import javascript unsafe "$2.slice($1)"
  js_slice1_wasm :: Int -> SomeJSArray m -> IO (SomeJSArray m1)
js_slice1    :: Int -> SomeJSArray m -> State# s -> (# State# s, SomeJSArray m1 #)
js_slice1 b x s = case unsafeIOToST (js_slice1_wasm b x) of ST f -> f s
#else
foreign import javascript unsafe "((x,y,z) => { z[x] = y; })"
  js_setIndex :: Int -> JSVal -> SomeJSArray m -> State# s -> (# State# s, () #)

foreign import javascript unsafe "((x,y,z) => { return z.slice(x,y); })"
  js_slice     :: Int -> Int -> SomeJSArray m -> State# s -> (# State# s, SomeJSArray m1 #)
foreign import javascript unsafe "((x,y) => { return y.slice(x); })"
  js_slice1    :: Int -> SomeJSArray m -> State# s -> (# State# s, SomeJSArray m1 #)
#endif

#if defined(wasm32_HOST_ARCH)
foreign import javascript unsafe "((x,y,z) => { return z.slice(x,y); })($1,$2,$3)"
#else
foreign import javascript unsafe "((x,y,z) => { return z.slice(x,y); })"
#endif
  js_slicePure  :: Int -> Int -> JSArray -> JSArray
#if defined(wasm32_HOST_ARCH)
foreign import javascript unsafe "((x,y) => { return y.slice(x); })($1,$2)"
#else
foreign import javascript unsafe "((x,y) => { return y.slice(x); })"
#endif
  js_slice1Pure :: Int -> JSArray -> JSArray

#if defined(wasm32_HOST_ARCH)
foreign import javascript unsafe "$1.concat($2)"
  js_append_wasm :: SomeJSArray m0 -> SomeJSArray m1 -> IO (SomeJSArray m2)
js_append   :: SomeJSArray m0 -> SomeJSArray m1 -> State# s ->  (# State# s, SomeJSArray m2 #)
js_append x y s = case unsafeIOToST (js_append_wasm x y) of ST f -> f s

foreign import javascript unsafe "$2.push($1);"
  js_push_wasm :: JSVal -> SomeJSArray m -> IO ()
js_push     :: JSVal -> SomeJSArray m -> State# s -> (# State# s, () #)
js_push e x s = case unsafeIOToST (js_push_wasm e x) of ST f -> f s
foreign import javascript unsafe "$1.pop()"
  js_pop_wasm :: SomeJSArray m -> IO JSVal
js_pop      :: SomeJSArray m -> State# s -> (# State# s, JSVal #)
js_pop x s = case unsafeIOToST (js_pop_wasm x) of ST f -> f s
foreign import javascript unsafe "$2.unshift($1);"
  js_unshift_wasm :: JSVal -> SomeJSArray m -> IO ()
js_unshift  :: JSVal -> SomeJSArray m -> State# s -> (# State# s, () #)
js_unshift e x s = case unsafeIOToST (js_unshift_wasm e x) of ST f -> f s
foreign import javascript unsafe "$1.shift()"
  js_shift_wasm :: SomeJSArray m -> IO JSVal
js_shift    :: SomeJSArray m -> State# s -> (# State# s, JSVal #)
js_shift x s = case unsafeIOToST (js_shift_wasm x) of ST f -> f s

foreign import javascript unsafe "$1.reverse();"
  js_reverse_wasm :: SomeJSArray m -> IO ()
js_reverse  :: SomeJSArray m -> State# s -> (# State# s, () #)
js_reverse x s = case unsafeIOToST (js_reverse_wasm x) of ST f -> f s

-- h$toHsListJSVal / h$fromHsListJSVal build / consume a Haskell list in
-- JavaScript, which is impossible on wasm.  Convert element by element
-- with GHC.JS.Prim.fromJSArray / toJSArray instead; the 'Exts.Any'
-- types are kept so that the unsafeCoerce-based callers are unchanged.
js_fromJSArray :: SomeJSArray m -> State# s -> (# State# s, Exts.Any #)
js_fromJSArray (SomeJSArray a) s =
  case unsafeIOToST (Prim.fromJSArray a) of
    ST f -> case f s of (# s', xs #) -> (# s', unsafeCoerce xs #)
js_fromJSArrayPure :: JSArray -> Exts.Any -- [JSVal]
js_fromJSArrayPure (SomeJSArray a) = unsafeCoerce (unsafePerformIO (Prim.fromJSArray a))
{-# NOINLINE js_fromJSArrayPure #-}

js_toJSArray :: Exts.Any -> State# s -> (# State# s, SomeJSArray m #)
js_toJSArray xs s =
  case unsafeIOToST (Prim.toJSArray (unsafeCoerce xs :: [JSVal])) of
    ST f -> case f s of (# s', a #) -> (# s', SomeJSArray a #)
js_toJSArrayPure :: Exts.Any -> JSArray
js_toJSArrayPure xs = SomeJSArray (unsafePerformIO (Prim.toJSArray (unsafeCoerce xs :: [JSVal])))
{-# NOINLINE js_toJSArrayPure #-}
#else
foreign import javascript unsafe "((x,y) => { return x.concat(y); })"
  js_append   :: SomeJSArray m0 -> SomeJSArray m1 -> State# s ->  (# State# s, SomeJSArray m2 #)

foreign import javascript unsafe "((x,y) => { y.push(x); })"
  js_push     :: JSVal -> SomeJSArray m -> State# s -> (# State# s, () #)
foreign import javascript unsafe "((x) => { return x.pop(); })"
  js_pop      :: SomeJSArray m -> State# s -> (# State# s, JSVal #)
foreign import javascript unsafe "((x,y) => { y.unshift(x); })"
  js_unshift  :: JSVal -> SomeJSArray m -> State# s -> (# State# s, () #)
foreign import javascript unsafe "((x) => { return x.shift(); })"
  js_shift    :: SomeJSArray m -> State# s -> (# State# s, JSVal #)

foreign import javascript unsafe "((x) => { return x.reverse(); })"
  js_reverse  :: SomeJSArray m -> State# s -> (# State# s, () #)

foreign import javascript unsafe "h$toHsListJSVal"
  js_fromJSArray :: SomeJSArray m -> State# s -> (# State# s, Exts.Any #)
foreign import javascript unsafe "h$toHsListJSVal"
  js_fromJSArrayPure :: JSArray -> Exts.Any -- [JSVal]

foreign import javascript unsafe "h$fromHsListJSVal"
  js_toJSArray :: Exts.Any -> State# s -> (# State# s, SomeJSArray m #)
foreign import javascript unsafe "h$fromHsListJSVal"
  js_toJSArrayPure :: Exts.Any -> JSArray
#endif

