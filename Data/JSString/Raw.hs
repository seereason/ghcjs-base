{-# LANGUAGE CPP #-}
{-# LANGUAGE ForeignFunctionInterface, JavaScriptFFI,
             MagicHash, UnboxedTuples, UnliftedFFITypes, GHCForeignImportPrim
  #-}

{-
  Low level bindings for JavaScript strings. These expose the underlying
  encoding. Use Data.JSString for 
 -}
module Data.JSString.Raw ( rawHead
                         , rawTail
                         , rawInit
                         , rawLast
                         , rawLength
                         , rawTake
                         , rawDrop
                         , rawTakeEnd
                         , rawDropEnd
                         , rawChunksOf
                         , rawChunksOf'
                         ) where

import           GHC.Exts
  ( Int(..), Int#, Char(..)
  , negateInt#
  , (+#), (-#), (>=#), (<#)
  , isTrue#, chr#)
import qualified GHC.Exts as Exts
import GHC.JS.Prim (JSVal)

import Unsafe.Coerce

import Data.JSString.Internal.Type


rawLength :: JSString -> Int
rawLength x = I# (js_length x)
{-# INLINE rawLength #-}

rawHead :: JSString -> Char
rawHead x
  | js_null x = emptyError "rawHead"
  | otherwise = C# (chr# (js_codePointAt 0# x))
{-# INLINE rawHead #-}

unsafeRawHead :: JSString -> Char
unsafeRawHead x = C# (chr# (js_codePointAt 0# x))
{-# INLINE unsafeRawHead #-}

rawLast :: JSString -> Char
rawLast x
  | js_null x = emptyError "rawLast"
  | otherwise = C# (chr# (js_charCodeAt (js_length x -# 1#) x))
{-# INLINE rawLast #-}

unsafeRawLast :: JSString -> Char
unsafeRawLast x = C# (chr# (js_charCodeAt (js_length x -# 1#) x))
{-# INLINE unsafeRawLast #-}

rawTail :: JSString -> JSString
rawTail x
  | js_null x = emptyError "rawTail"
  | otherwise = JSString $ js_tail x
{-# INLINE rawTail #-}

unsafeRawTail :: JSString -> JSString
unsafeRawTail x = JSString $ js_tail x
{-# INLINE unsafeRawTail #-}

rawInit :: JSString -> JSString
rawInit x = js_substr 0# (js_length x -# 1#) x
{-# INLINE rawInit #-}

unsafeRawInit :: JSString -> JSString
unsafeRawInit x = js_substr 0# (js_length x -# 1#) x
{-# INLINE unsafeRawInit #-}

unsafeRawIndex :: Int -> JSString -> Char
unsafeRawIndex (I# n) x = C# (chr# (js_charCodeAt n x))
{-# INLINE unsafeRawIndex #-}

rawIndex :: Int -> JSString -> Char
rawIndex (I# n) x
  | isTrue# (n <# 0#) || isTrue# (n >=# js_length x) =
      overflowError "rawIndex"
  | otherwise = C# (chr# (js_charCodeAt n x))
{-# INLINE rawIndex #-}
    
rawTake :: Int -> JSString -> JSString
rawTake (I# n) x = js_substr 0# n x
{-# INLINE rawTake #-}

rawDrop :: Int -> JSString -> JSString
rawDrop (I# n) x = js_substr1 n x
{-# INLINE rawDrop #-}

rawTakeEnd :: Int -> JSString -> JSString
rawTakeEnd (I# k) x = js_slice1 (negateInt# k) x
{-# INLINE rawTakeEnd #-}

rawDropEnd :: Int -> JSString -> JSString
rawDropEnd (I# k) x = js_substr 0# (js_length x -# k) x
{-# INLINE rawDropEnd #-}

rawChunksOf :: Int -> JSString -> [JSString]
rawChunksOf (I# k) x =
  let l     = js_length x
      go i = case i >=# l of
               0# -> js_substr i k x : go (i +# k)
               _  -> []
  in go 0#
{-# INLINE rawChunksOf #-}

rawChunksOf' :: Int -> JSString -> [JSString]
rawChunksOf' (I# k) x = unsafeCoerce (js_rawChunksOf k x)
{-# INLINE rawChunksOf' #-}

rawSplitAt :: Int -> JSString -> (JSString, JSString)
rawSplitAt (I# k) x = (js_substr 0# k x, js_substr1 k x)
{-# INLINE rawSplitAt #-}

emptyError :: String -> a
emptyError fun = error $ "Data.JSString.Raw." ++ fun ++ ": empty input"

overflowError :: String -> a
overflowError fun = error $ "Data.JSString.Raw." ++ fun ++ ": size overflow"

-- -----------------------------------------------------------------------------

#if defined(wasm32_HOST_ARCH)
foreign import javascript unsafe "((x) => { return x===''; })($1)" js_null :: JSString -> Bool
#else
foreign import javascript unsafe
  "((x) => { return x===''; })" js_null :: JSString -> Bool
#endif
#if defined(wasm32_HOST_ARCH)
foreign import javascript unsafe "((x) => { return x.length; })($1)"
  js_length_wasm :: JSString -> Int
js_length :: JSString -> Int#
js_length a1 = case js_length_wasm a1 of I# r -> r
{-# INLINE js_length #-}
#else
foreign import javascript unsafe
  "((x) => { return x.length; })" js_length :: JSString -> Int#
#endif
#if defined(wasm32_HOST_ARCH)
foreign import javascript unsafe "((x,y,z) => { return z.substr(x,y); })($1,$2,$3)"
  js_substr_wasm :: Int -> Int -> JSString -> JSString
js_substr :: Int# -> Int# -> JSString -> JSString
js_substr a1 a2 a3 = js_substr_wasm (I# a1) (I# a2) a3
{-# INLINE js_substr #-}
#else
foreign import javascript unsafe
  "((x,y,z) => { return z.substr(x,y); })" js_substr :: Int# -> Int# -> JSString -> JSString
#endif
#if defined(wasm32_HOST_ARCH)
foreign import javascript unsafe "((x,y) => { return y.substr(x); })($1,$2)"
  js_substr1_wasm :: Int -> JSString -> JSString
js_substr1 :: Int# -> JSString -> JSString
js_substr1 a1 a2 = js_substr1_wasm (I# a1) a2
{-# INLINE js_substr1 #-}
#else
foreign import javascript unsafe
  "((x,y) => { return y.substr(x); })" js_substr1 :: Int# -> JSString -> JSString
#endif
#if defined(wasm32_HOST_ARCH)
foreign import javascript unsafe "((x,y,z) => { return z.slice(x,y); })($1,$2,$3)"
  js_slice_wasm :: Int -> Int -> JSString -> JSString
js_slice :: Int# -> Int# -> JSString -> JSString
js_slice a1 a2 a3 = js_slice_wasm (I# a1) (I# a2) a3
{-# INLINE js_slice #-}
#else
foreign import javascript unsafe
  "((x,y,z) => { return z.slice(x,y); })" js_slice :: Int# -> Int# -> JSString -> JSString
#endif
#if defined(wasm32_HOST_ARCH)
foreign import javascript unsafe "((x,y) => { return y.slice(x); })($1,$2)"
  js_slice1_wasm :: Int -> JSString -> JSString
js_slice1 :: Int# -> JSString -> JSString
js_slice1 a1 a2 = js_slice1_wasm (I# a1) a2
{-# INLINE js_slice1 #-}
#else
foreign import javascript unsafe
  "((x,y) => { return y.slice(x); })" js_slice1 :: Int# -> JSString -> JSString
#endif
#if defined(wasm32_HOST_ARCH)
foreign import javascript unsafe "((x,y,z) => { return z.indexOf(x,y); })($1,$2,$3)"
  js_indexOf_wasm :: JSString -> Int -> JSString -> Int
js_indexOf :: JSString -> Int# -> JSString -> Int#
js_indexOf a1 a2 a3 = case js_indexOf_wasm a1 (I# a2) a3 of I# r -> r
{-# INLINE js_indexOf #-}
#else
foreign import javascript unsafe
  "((x,y,z) => { return z.indexOf(x,y); })" js_indexOf :: JSString -> Int# -> JSString -> Int#
#endif
#if defined(wasm32_HOST_ARCH)
foreign import javascript unsafe "((x,y) => { return y.indexOf(x); })($1,$2)"
  js_indexOf1_wasm :: JSString -> JSString -> Int
js_indexOf1 :: JSString -> JSString -> Int#
js_indexOf1 a1 a2 = case js_indexOf1_wasm a1 a2 of I# r -> r
{-# INLINE js_indexOf1 #-}
#else
foreign import javascript unsafe
  "((x,y) => { return y.indexOf(x); })" js_indexOf1 :: JSString -> JSString -> Int#
#endif
#if defined(wasm32_HOST_ARCH)
foreign import javascript unsafe "((x,y) => { return y.charCodeAt(x); })($1,$2)"
  js_charCodeAt_wasm :: Int -> JSString -> Int
js_charCodeAt :: Int# -> JSString -> Int#
js_charCodeAt a1 a2 = case js_charCodeAt_wasm (I# a1) a2 of I# r -> r
{-# INLINE js_charCodeAt #-}
#else
foreign import javascript unsafe
  "((x,y) => { return y.charCodeAt(x); })" js_charCodeAt :: Int# -> JSString -> Int#
#endif
#if defined(wasm32_HOST_ARCH)
foreign import javascript unsafe "((x,y) => { return y.codePointAt(x); })($1,$2)"
  js_codePointAt_wasm :: Int -> JSString -> Int
js_codePointAt :: Int# -> JSString -> Int#
js_codePointAt a1 a2 = case js_codePointAt_wasm (I# a1) a2 of I# r -> r
{-# INLINE js_codePointAt #-}
#else
foreign import javascript unsafe
  "((x,y) => { return y.codePointAt(x); })" js_codePointAt :: Int# -> JSString -> Int#
#endif
#if defined(wasm32_HOST_ARCH)
-- The JavaScript backend's "$hsRawChunksOf" names no existing function (and
-- h$jsstringRawChunksOf in jsbits is broken); this implements the intended
-- behaviour, the same as 'rawChunksOf': chunks of k UTF-16 code units.
foreign import javascript unsafe
  "var k = $1, x = $2, l = x.length, a = []; if(l === 0) return a; if(l <= k || k <= 0) return [x]; for(var i = 0; i < l; i += k) a.push(x.substr(i, k)); return a;"
  js_rawChunksOf_wasm :: Int -> JSString -> JSVal
foreign import javascript unsafe "$1.length" js_rawArrLen :: JSVal -> Int
foreign import javascript unsafe "$1[$2]"    js_rawArrVal :: JSVal -> Int -> JSVal
js_rawChunksOf :: Int# -> JSString -> Exts.Any -- [JSString]
js_rawChunksOf k x = unsafeCoerce (go 0)
  where a = js_rawChunksOf_wasm (I# k) x
        n = js_rawArrLen a
        go :: Int -> [JSString]
        go i | i >= n    = []
             | otherwise = JSString (js_rawArrVal a i) : go (i + 1)
foreign import javascript unsafe
  "var s = $1, l = s.length; if(l===0) return null; var ch = s.codePointAt(0); if(ch === undefined) return null; return s.substr((ch>=0x10000)?2:1);"
  js_tail :: JSString -> JSVal -- null for empty string
#else
foreign import javascript unsafe
  "$hsRawChunksOf" js_rawChunksOf :: Int# -> JSString -> Exts.Any -- [JSString]
foreign import javascript unsafe
  "h$jsstringTail" js_tail :: JSString -> JSVal -- null for empty string
#endif

