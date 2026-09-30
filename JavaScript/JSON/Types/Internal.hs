{-# LANGUAGE CPP #-}
{-# LANGUAGE ForeignFunctionInterface, JavaScriptFFI, EmptyDataDecls,
             DeriveDataTypeable, GHCForeignImportPrim, DataKinds, KindSignatures,
             PolyKinds, MultiParamTypeClasses, TypeSynonymInstances, FlexibleInstances,
             UnboxedTuples, MagicHash, UnliftedFFITypes
  #-}

module JavaScript.JSON.Types.Internal
    ( -- * Core JSON types
      SomeValue(..),  Value,  MutableValue
    , SomeValue'(..), Value', MutableValue'
    , emptyArray, isEmptyArray
    , Pair
    , Object, MutableObject
    , objectProperties, objectPropertiesIO
    , objectAssocs,     objectAssocsIO
    , Lookup(..), IOLookup(..)
    , emptyObject
    , match
    , arrayValue, stringValue, doubleValue, nullValue, boolValue, objectValue
    , arrayValueList, indexV
      {-  fixme implement freezing / thawing
    , freeze, unsafeFreeze
    , thaw,   unsafeThaw
       -}
      -- * Type conversion
    , Parser
    , Result(..)
    , parse
    , parseEither
    , parseMaybe
    , modifyFailure
    , encode
      -- * Constructors and accessors
    , object

      -- * Generic and TH encoding configuration
    , Options(..)
    , SumEncoding(..)
    , defaultOptions
    , defaultTaggedObject
      
      -- * Used for changing CamelCase names into something else.
    , camelTo
      -- * Other types
    , DotNetTime(..)
    ) where

import Data.Aeson.Types
  ( Parser, Result(..)
  , parse, parseEither, parseMaybe, modifyFailure
  , Options(..), SumEncoding(..), defaultOptions, defaultTaggedObject
  , camelTo
  , DotNetTime(..)
  )

import           Prelude           hiding (lookup)

import           Control.DeepSeq
import           Control.Exception

import           Data.Coerce
import           Data.Data
import qualified Data.JSString     as JSS
import           Data.JSString.Internal.Type (JSString(..))
import           Data.Maybe (fromMaybe)
import           Data.Typeable

import qualified GHC.Exts          as Exts
import           GHC.Types (IO(..))

import qualified GHCJS.Foreign     as F
import           GHCJS.Internal.Types
import           GHCJS.Types
#if defined(wasm32_HOST_ARCH)
import qualified GHC.JS.Prim as Prim
import           System.IO.Unsafe (unsafePerformIO)
#else
import qualified GHC.JS.Prim.Internal.Build as IB
#endif

import qualified JavaScript.Array          as A
import qualified JavaScript.Array.Internal as AI

import           Unsafe.Coerce

data JSONException = UnknownKey
  deriving (Show, Typeable)

instance Exception JSONException

-- any JSON value
newtype SomeValue (m :: MutabilityType s) =
  SomeValue JSVal deriving (Typeable)
type Value        = SomeValue Immutable
type MutableValue = SomeValue Mutable
instance NFData (SomeValue (m :: MutabilityType s)) where
  rnf (SomeValue v) = rnf v

-- a dictionary (object)
newtype SomeObject (m :: MutabilityType s) =
  SomeObject JSVal deriving (Typeable)
type Object        = SomeObject Immutable
type MutableObject = SomeObject Mutable
instance NFData (SomeObject (m :: MutabilityType s)) where
  rnf (SomeObject v) = rnf v

{-
objectFromAssocs :: [(JSString, Value)] -> Object
objectFromAssocs xs = rnf xs `seq` js_objectFromAssocs (unsafeCoerce xs)
{-# INLINE objectFromAssocs #-}
-}

objectProperties :: Object -> AI.JSArray
objectProperties o = js_objectPropertiesPure o
{-# INLINE objectProperties #-}

objectPropertiesIO :: SomeObject o -> IO AI.JSArray
objectPropertiesIO o = js_objectProperties o
{-# INLINE objectPropertiesIO #-}

#if defined(wasm32_HOST_ARCH)
objectAssocs :: Object -> [(JSString, Value)]
objectAssocs o = unsafePerformIO (listAssocsW o)
{-# INLINE objectAssocs #-}

objectAssocsIO :: SomeObject m -> IO [(JSString, Value)]
objectAssocsIO o = listAssocsW o
{-# INLINE objectAssocsIO #-}
#else
objectAssocs :: Object -> [(JSString, Value)]
objectAssocs o = unsafeCoerce (js_listAssocsPure o)
{-# INLINE objectAssocs #-}

objectAssocsIO :: SomeObject m -> IO [(JSString, Value)]
objectAssocsIO o = IO $ \s -> case js_listAssocs o s of
                                (# s', r #) -> (# s', unsafeCoerce r #)
{-# INLINE objectAssocsIO #-}
#endif

type Pair        = (JSString, Value)
type MutablePair = (JSString, MutableValue)

data SomeValue' (m :: MutabilityType s)
  = Object !(SomeObject m)
  | Array  !(AI.SomeJSArray m)
  | String !JSString
  | Number !Double
  | Bool   !Bool
  | Null
  deriving (Typeable)

type Value'        = SomeValue' Immutable
type MutableValue' = SomeValue' Mutable

-- -----------------------------------------------------------------------------
-- immutable lookup

class Lookup k a where
  (!)       :: k -> a -> Value             -- ^ throws when result is not a JSON value
  lookup    :: k -> a -> Maybe Value       -- ^ returns Nothing when result is not a JSON value
-- fixme more optimized matching
--  lookup'   :: k -> a -> Maybe Value'      -- ^ returns Nothing when result is not a JSON value

instance Lookup JSString Object where
  p ! d      = fromMaybe (throw UnknownKey) (lookup p d)
  lookup p d = let v = js_lookupDictPure p d
               in  if isUndefined v then Nothing else Just (SomeValue v)

instance Lookup JSString Value where
  p ! d      = fromMaybe (throw UnknownKey) (lookup p d)
  lookup p d = let v = js_lookupDictPureSafe p d
               in if isUndefined v then Nothing else Just (SomeValue v)

instance Lookup Int A.JSArray where
  i ! a      = fromMaybe (throw UnknownKey) (lookup i a)
  lookup i a = let v = js_lookupArrayPure i a
               in if isUndefined v then Nothing else Just (SomeValue v)
                                                     
instance Lookup Int Value where
  i ! a      = fromMaybe (throw UnknownKey) (lookup i a)
  lookup i a = let v = js_lookupArrayPureSafe i a
               in if isUndefined v then Nothing else Just (SomeValue v)

-- -----------------------------------------------------------------------------
-- mutable lookup

class IOLookup k a where
  (^!)      :: k -> a -> IO MutableValue          -- ^ throws when result is not a JSON value
  lookupIO  :: k -> a -> IO (Maybe MutableValue)  -- ^ returns Nothing when result is not a JSON value
  lookupIO' :: k -> a -> IO (Maybe MutableValue') -- ^ returns Nothing when result is not a JSON value

-- -----------------------------------------------------------------------------

match :: SomeValue m -> SomeValue' m
match (SomeValue v) =
  case F.jsonTypeOf v of
    F.JSONNull    -> Null
    F.JSONBool    -> Bool   (js_jsvalToBool v)
    F.JSONInteger -> Number (js_jsvalToDouble v)
    F.JSONFloat   -> Number (js_jsvalToDouble v)
    F.JSONString  -> String (JSString v)
    F.JSONArray   -> Array  (AI.SomeJSArray v)
    F.JSONObject  -> Object (SomeObject v)
{-# INLINE match #-}

emptyArray :: Value
emptyArray = js_emptyArray
{-# INLINE emptyArray #-}

isEmptyArray :: Value -> Bool
isEmptyArray v = js_isEmptyArray v
{-# INLINE isEmptyArray #-}

emptyObject :: Object
emptyObject = js_emptyObject
{-# INLINE emptyObject #-}

object :: [Pair] -> Object
object []      = js_emptyObject
#if defined(wasm32_HOST_ARCH)
-- GHC.JS.Prim.Internal.Build does not exist on wasm
object xs      = SomeObject $ unsafePerformIO $ do
  o <- js_newObject
  mapM_ (\(JSString k, SomeValue v) -> js_setObjProp o k v) xs
  return o
#else
object xs      = SomeObject (IB.buildObjectI $ coerce xs)
#endif
{-# INLINE object #-}

freeze :: MutableValue -> IO Value
freeze v = js_clone v
{-# INLINE freeze #-}

unsafeFreeze :: MutableValue -> IO Value
unsafeFreeze (SomeValue v) = pure (SomeValue v)
{-# INLINE unsafeFreeze #-}

thaw :: Value -> IO MutableValue
thaw v = js_clone v
{-# INLINE thaw #-}

unsafeThaw :: Value -> IO MutableValue
unsafeThaw (SomeValue v) = pure (SomeValue v)
{-# INLINE unsafeThaw #-}

-- -----------------------------------------------------------------------------
-- smart constructors

arrayValue :: AI.JSArray -> Value
arrayValue (AI.SomeJSArray a) = SomeValue a
{-# INLINE arrayValue #-}

stringValue :: JSString -> Value
stringValue (JSString x) = SomeValue x
{-# INLINE stringValue #-}

doubleValue :: Double -> Value
doubleValue d = SomeValue (js_doubleToJSVal d)
{-# INLINE doubleValue #-}

boolValue :: Bool -> Value
boolValue True  = js_trueValue
boolValue False = js_falseValue
{-# INLINE boolValue #-}

nullValue :: Value
nullValue = SomeValue F.jsNull

arrayValueList :: [Value] -> AI.JSArray
arrayValueList xs = A.fromList (coerce xs)
{-# INLINE arrayValueList #-}

indexV :: AI.JSArray -> Int -> Value
indexV a i = SomeValue (AI.index i a)
{-# INLINE indexV #-}

objectValue :: Object -> Value
objectValue (SomeObject o) = SomeValue o
{-# INLINE objectValue #-}

encode :: Value -> JSString
encode v = js_encode v
{-# INLINE encode #-}

-- -----------------------------------------------------------------------------

#if defined(wasm32_HOST_ARCH)
foreign import javascript unsafe "(() => { return []; })()" js_emptyArray :: Value
#else
foreign import javascript unsafe
  "(() => { return []; })" js_emptyArray :: Value
#endif
#if defined(wasm32_HOST_ARCH)
foreign import javascript unsafe "(() => { return {}; })()" js_emptyObject :: Object
#else
foreign import javascript unsafe
  "(() => { return {}; })" js_emptyObject :: Object
#endif
#if defined(wasm32_HOST_ARCH)
foreign import javascript unsafe "((x) => { return x.length === 0; })($1)" js_isEmptyArray :: Value -> Bool
#else
foreign import javascript unsafe
  "((x) => { return x.length === 0; })" js_isEmptyArray :: Value -> Bool
#endif

#if defined(wasm32_HOST_ARCH)
foreign import javascript unsafe "(() => { return true; })()" js_trueValue :: Value
#else
foreign import javascript unsafe
  "(() => { return true; })" js_trueValue :: Value
#endif
#if defined(wasm32_HOST_ARCH)
foreign import javascript unsafe "(() => { return false; })()" js_falseValue :: Value
#else
foreign import javascript unsafe
  "(() => { return false; })" js_falseValue :: Value
#endif

-- -----------------------------------------------------------------------------
-- types must be checked before using these conversions

#if defined(wasm32_HOST_ARCH)
foreign import javascript unsafe "((x) => { return x; })($1)" js_jsvalToDouble :: JSVal -> Double
#else
foreign import javascript unsafe
  "((x) => { return x; })" js_jsvalToDouble :: JSVal -> Double
#endif
#if defined(wasm32_HOST_ARCH)
foreign import javascript unsafe "((x) => { return x; })($1)" js_jsvalToBool   :: JSVal -> Bool
#else
foreign import javascript unsafe
  "((x) => { return x; })" js_jsvalToBool   :: JSVal -> Bool
#endif

-- -----------------------------------------------------------------------------
-- various lookups

#if defined(wasm32_HOST_ARCH)
foreign import javascript unsafe "((x,y) => { return y[x]; })($1,$2)"
#else
foreign import javascript unsafe
  "((x,y) => { return y[x]; })"
#endif
  js_lookupDictPure :: JSString -> Object -> JSVal

#if defined(wasm32_HOST_ARCH)
foreign import javascript unsafe "((x,y) => { return typeof(y) === 'object' ? y[x] : undefined; })($1,$2)"
#else
foreign import javascript unsafe
  "((x,y) => { return typeof(y) === 'object' ? y[x] : undefined; })"
#endif
  js_lookupDictPureSafe :: JSString -> Value -> JSVal

#if defined(wasm32_HOST_ARCH)
foreign import javascript unsafe "((x,y) => { return y[x]; })($1,$2)" js_lookupArrayPure :: Int -> A.JSArray -> JSVal
#else
foreign import javascript unsafe
  "((x,y) => { return y[x]; })" js_lookupArrayPure :: Int -> A.JSArray -> JSVal
#endif
#if defined(wasm32_HOST_ARCH)
foreign import javascript unsafe
  "Array.isArray($2) ? $2[$1] : undefined"
  js_lookupArrayPureSafe :: Int -> Value -> JSVal
#else
foreign import javascript unsafe
  "((x,y) => { return h$isArray(y) ? y[x] : undefined; })"
  js_lookupArrayPureSafe :: Int -> Value -> JSVal
#endif
#if defined(wasm32_HOST_ARCH)
foreign import javascript unsafe "((x) => { return x; })($1)"
#else
foreign import javascript unsafe
  "((x) => { return x; })"
#endif
  js_doubleToJSVal :: Double -> JSVal

#if defined(wasm32_HOST_ARCH)
foreign import javascript unsafe "((x) => { return JSON.decode(JSON.encode(x)); })($1)"
#else
foreign import javascript unsafe
  "((x) => { return JSON.decode(JSON.encode(x)); })"
#endif
  js_clone :: SomeValue m0 -> IO (SomeValue m1)

-- -----------------------------------------------------------------------------

#if defined(wasm32_HOST_ARCH)
-- h$allProps (jsbits/utils.js)
foreign import javascript unsafe
  "var a = [], i = 0; for(var p in $1) a[i++] = p; return a;"
  js_objectPropertiesPure :: Object -> AI.JSArray
foreign import javascript unsafe
  "var a = [], i = 0; for(var p in $1) a[i++] = p; return a;"
  js_objectProperties :: SomeObject m -> IO AI.JSArray

-- h$listAssocs builds a Haskell list, which cannot be done in JavaScript
-- on wasm: return [k1,v1,k2,v2,...] in for-in order instead.
foreign import javascript unsafe
  "var a = []; for(var p in $1) a.push(p, $1[p]); return a;"
  js_listAssocsFlat :: SomeObject m -> IO JSVal

-- h$listAssocs conses each pair onto the list, so the result is in
-- reverse for-in order
listAssocsW :: SomeObject m -> IO [(JSString, SomeValue m')]
listAssocsW o = do
  xs <- Prim.fromJSArray =<< js_listAssocsFlat o
  let pairs (k:v:r) = (JSString k, SomeValue v) : pairs r
      pairs _       = []
  return (reverse (pairs xs))

-- for 'object': h$buildObjectFromTupList, later keys override earlier ones
foreign import javascript unsafe "({})" js_newObject :: IO JSVal
foreign import javascript unsafe "$1[$2] = $3" js_setObjProp :: JSVal -> JSVal -> JSVal -> IO ()
#else
foreign import javascript unsafe
  "h$allProps"
  js_objectPropertiesPure :: Object -> AI.JSArray
foreign import javascript unsafe
  "h$allProps"
  js_objectProperties :: SomeObject m -> IO AI.JSArray

foreign import javascript unsafe
  "h$listAssocs"
  js_listAssocsPure :: Object -> Exts.Any -- [(JSString, Value)]
foreign import javascript unsafe
  "h$listAssocs"
  js_listAssocs :: SomeObject m -> Exts.State# s -> (# Exts.State# s, Exts.Any {- [(JSString, Value)] -} #)
#endif

#if defined(wasm32_HOST_ARCH)
foreign import javascript unsafe "JSON.stringify($1)"
#else
foreign import javascript unsafe
  "JSON.stringify"
#endif
  js_encode :: Value -> JSString
