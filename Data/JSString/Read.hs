{-# LANGUAGE CPP #-}
{-# LANGUAGE ForeignFunctionInterface, JavaScriptFFI, UnliftedFFITypes,
             GHCForeignImportPrim, UnboxedTuples, BangPatterns,
             MagicHash
  #-}
module Data.JSString.Read ( isInteger
                          , isNatural
                          , readInt
                          , readIntMaybe
                          , lenientReadInt
                          , readInt64
                          , readInt64Maybe
                          , readWord64
                          , readWord64Maybe
                          , readDouble
                          , readDoubleMaybe
                          , readInteger
                          , readIntegerMaybe
                          ) where

import GHCJS.Types

import GHC.Exts (Any, Int#, Int64#, Word64#, Int(..))
import GHC.Int (Int64(..))
import GHC.Word (Word64(..))
import Unsafe.Coerce
import Data.Maybe
import Data.JSString
#if defined(wasm32_HOST_ARCH)
import GHC.Exts (intToInt64#, wordToWord64#)
import qualified GHC.JS.Prim as Prim
import Data.JSString.Internal.Type (JSString(..))
#endif

{- |
    Returns whether the JSString represents an integer at base 10
 -}
isInteger :: JSString -> Bool
isInteger j = js_isInteger j
{-# INLINE isInteger #-}

{- |
    Returns whether the JSString represents a natural number at base 10
    (including 0)
 -}
isNatural :: JSString -> Bool
isNatural j = js_isInteger j
{-# INLINE isNatural #-}

{- |
     Convert a JSString to an Int, throwing an exception if it cannot
     be converted. Leading spaces are allowed. The function ignores
     trailing non-digit characters.
 -}
lenientReadInt :: JSString -> Int
lenientReadInt j = fromMaybe (readError "lenientReadInt") (lenientReadIntMaybe j)
{-# INLINE lenientReadInt #-}

{- |
     Convert a JSString to an Int, returning Nothing if it cannot
     be converted. Leading spaces are allowed. The function ignores
     trailing non-digit characters.
 -}
lenientReadIntMaybe :: JSString -> Maybe Int
lenientReadIntMaybe j = convertNullMaybe js_lenientReadInt j
{-# INLINE lenientReadIntMaybe #-}

{- |
     Convert a JSString to an Int, throwing an exception if it cannot
     be converted. Leading spaces and trailing non-digit characters
     are not allowed.
 -}
readInt :: JSString -> Int
readInt j = fromMaybe (readError "readInt") (readIntMaybe j)
{-# INLINE readInt #-}

{- |
     Convert a JSString to an Int, returning Nothing if it cannot
     be converted. Leading spaces and trailing non-digit characters
     are not allowed.
 -}
readIntMaybe :: JSString -> Maybe Int
readIntMaybe j = convertNullMaybe js_readInt j
{-# INLINE readIntMaybe #-}

readInt64 :: JSString -> Int64
readInt64 j = fromMaybe (readError "readInt64") (readInt64Maybe j)
{-# INLINE readInt64 #-}

readInt64Maybe :: JSString -> Maybe Int64
readInt64Maybe j = case js_readInt64 j of
                     (# 0#, _ #) -> Nothing
                     (#  _, x #) -> Just (I64# x)
{-# INLINE readInt64Maybe #-}

readWord64 :: JSString -> Word64
readWord64 j = fromMaybe (readError "readWord64") (readWord64Maybe j)
{-# INLINE readWord64 #-}

readWord64Maybe :: JSString -> Maybe Word64
readWord64Maybe j = case js_readWord64 j of
                     (# 0#, _ #) -> Nothing
                     (#  _, x #) -> Just (W64# x)
{-# INLINE readWord64Maybe #-}


{- |
     Convert a JSString to an Int, throwing an exception if it cannot
     be converted. Leading spaces are allowed. The function ignores
     trailing non-digit characters.
 -}
readDouble :: JSString -> Double
readDouble j = fromMaybe (readError "readDouble") (readDoubleMaybe j)
{-# INLINE readDouble #-}

{- |
     Convert a JSString to a Double, returning Nothing if it cannot
     be converted. Leading spaces are allowed. The function ignores
     trailing non-digit characters.
 -}

readDoubleMaybe :: JSString -> Maybe Double
readDoubleMaybe j = convertNullMaybe js_readDouble j
{-# INLINE readDoubleMaybe #-}

{- |
     Convert a JSString to a Double, returning Nothing if it cannot
     be converted. Leading spaces and trailing non-digit characters
     are not allowed.
 -}
strictReadDoubleMaybe :: JSString -> Maybe Double
strictReadDoubleMaybe j = convertNullMaybe js_readDouble j
{-# INLINE strictReadDoubleMaybe #-}

readInteger :: JSString -> Integer
readInteger j = fromMaybe (readError "readInteger") (readIntegerMaybe j)
{-# INLINE readInteger #-}

readIntegerMaybe :: JSString -> Maybe Integer
readIntegerMaybe j = convertNullMaybe js_readInteger j
{-# INLINE readIntegerMaybe #-}

-- ----------------------------------------------------------------------------

#if defined(wasm32_HOST_ARCH)
-- On wasm a JavaScript number is not a Haskell heap object, so the
-- non-null result is converted explicitly for each result type.
class ReadResult a where
  fromReadResult :: JSVal -> a

instance ReadResult Int where
  fromReadResult = js_jsvalToInt

instance ReadResult Double where
  fromReadResult = js_jsvalToDouble

-- js_readInteger returns the (validated) string itself on wasm
instance ReadResult Integer where
  fromReadResult r = read (Prim.fromJSString r)

convertNullMaybe :: ReadResult a => (JSString -> JSVal) -> JSString -> Maybe a
convertNullMaybe f j
  | js_isNull r = Nothing
  | otherwise   = Just (fromReadResult r)
  where
    r = f j
{-# INLINE convertNullMaybe #-}
#else
convertNullMaybe :: (JSString -> JSVal) -> JSString -> Maybe a
convertNullMaybe f j
  | js_isNull r = Nothing
  | otherwise   = Just (unsafeCoerce (js_toHeapObject r))
  where
    r = f j
{-# INLINE convertNullMaybe #-}
#endif

readError :: String -> a
readError xs = error ("Data.JSString.Read." ++ xs)

-- ----------------------------------------------------------------------------

#if defined(wasm32_HOST_ARCH)
foreign import javascript unsafe "((x) => { return x===null; })($1)" js_isNull :: JSVal -> Bool
#else
foreign import javascript unsafe
  "((x) => { return x===null; })" js_isNull :: JSVal -> Bool
#endif
#if defined(wasm32_HOST_ARCH)
foreign import javascript unsafe "$1|0" js_jsvalToInt :: JSVal -> Int
foreign import javascript unsafe "$1" js_jsvalToDouble :: JSVal -> Double
-- returns the string itself (or null), see the ReadResult Integer instance
foreign import javascript unsafe
  "/^(-)?\\d+$/.test($1) ? $1 : null" js_readInteger :: JSString -> JSVal
foreign import javascript unsafe
  "if(!/^-?\\d+/.test($1)) return null; var x = parseInt($1, 10); var x0 = x|0; return (x===x0) ? x0 : null;"
  js_readInt :: JSString -> JSVal
foreign import javascript unsafe
  "var x = parseInt($1, 10); var x0 = x|0; return (x===x0) ? x0 : null;"
  js_lenientReadInt :: JSString -> JSVal
-- the 64 bit values are BigInts (null if the string is not a number),
-- wrapped to 64 bits like the JavaScript backend's goog.math.Long code
foreign import javascript unsafe
  "/^(-)?\\d+$/.test($1) ? BigInt.asIntN(64, BigInt($1)) : null"
  js_readInt64_wasm :: JSString -> JSVal
foreign import javascript unsafe
  "/^\\d+$/.test($1) ? BigInt.asUintN(64, BigInt($1)) : null"
  js_readWord64_wasm :: JSString -> JSVal
foreign import javascript unsafe "$1" js_jsvalToInt64  :: JSVal -> Int64
foreign import javascript unsafe "$1" js_jsvalToWord64 :: JSVal -> Word64
js_readInt64 :: JSString -> (# Int#, Int64# #)
js_readInt64 j =
  let r = js_readInt64_wasm j
  in  if js_isNull r
        then (# 0#, intToInt64# 0# #)
        else case js_jsvalToInt64 r of I64# x -> (# 1#, x #)
{-# INLINE js_readInt64 #-}
js_readWord64 :: JSString -> (# Int#, Word64# #)
js_readWord64 j =
  let r = js_readWord64_wasm j
  in  if js_isNull r
        then (# 0#, wordToWord64# 0## #)
        else case js_jsvalToWord64 r of W64# x -> (# 1#, x #)
{-# INLINE js_readWord64 #-}
foreign import javascript unsafe
  "parseFloat($1, 10)" js_readDouble :: JSString -> JSVal
foreign import javascript unsafe
  "/^-?\\d+$/.test($1)" js_isInteger :: JSString -> Bool
foreign import javascript unsafe
  "/^\\d+$/.test($1)" js_isNatural :: JSString -> Bool
#else
foreign import javascript unsafe
  "((x) => { return x; })" js_toHeapObject :: JSVal -> Any
foreign import javascript unsafe
  "h$jsstringReadInteger" js_readInteger :: JSString -> JSVal
foreign import javascript unsafe
  "h$jsstringReadInt" js_readInt :: JSString -> JSVal
foreign import javascript unsafe
  "h$jsstringLenientReadInt" js_lenientReadInt :: JSString -> JSVal
foreign import javascript unsafe
  "h$jsstringReadInt64" js_readInt64 :: JSString -> (# Int#, Int64# #)
foreign import javascript unsafe
  "h$jsstringReadWord64" js_readWord64 :: JSString -> (# Int#, Word64# #)
foreign import javascript unsafe
  "h$jsstringReadDouble" js_readDouble :: JSString -> JSVal
foreign import javascript unsafe
  "h$jsstringIsInteger" js_isInteger :: JSString -> Bool
foreign import javascript unsafe
  "h$jsstringIsNatural" js_isNatural :: JSString -> Bool
#endif
