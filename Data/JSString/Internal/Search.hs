{-# LANGUAGE CPP #-}
{-# LANGUAGE MagicHash, BangPatterns, UnboxedTuples, TypeFamilies,
             ForeignFunctionInterface, JavaScriptFFI, UnliftedFFITypes,
             GHCForeignImportPrim
  #-}

module Data.JSString.Internal.Search ( indices
                                     ) where

import GHC.Exts (Int#, (+#), Int(..))
import Data.JSString
#if defined(wasm32_HOST_ARCH)
import Data.JSString.Internal.Type (JSString(..))
import GHC.JS.Prim (JSVal)
#endif

indices :: JSString -> JSString -> [Int]
indices needle haystack = go 0# 0#
  where
    go n i = case js_indexOf needle n i haystack of
             (# -1#, _  #) -> []
             (# n' , i' #) -> I# n' : go (n' +# 1#) (i' +# 1#) 

#if defined(wasm32_HOST_ARCH)
-- h$jsstringIndices inlined; returns [n, endI] ([-1, -1] if not found)
foreign import javascript unsafe
  "var needle = $1, startN = $2, startI = $3, haystack = $4; var endI = haystack.indexOf(needle, startI); if(endI === -1) return [-1, -1]; var n = startN; for(var i = startI; i < endI; i++) { if(!((haystack.charCodeAt(i)|1023)===0xDFFF)) n++; } return [n, endI];"
  js_indexOf_wasm :: JSString -> Int -> Int -> JSString -> JSVal
foreign import javascript unsafe "$1[$2]" js_arrInt :: JSVal -> Int -> Int
js_indexOf :: JSString -> Int# -> Int# -> JSString -> (# Int#, Int# #)
js_indexOf needle n i haystack =
  let r = js_indexOf_wasm needle (I# n) (I# i) haystack
  in  case js_arrInt r 0 of
        I# n' -> case js_arrInt r 1 of
                   I# i' -> (# n', i' #)
#else
foreign import javascript unsafe
  "h$jsstringIndices"
  js_indexOf :: JSString -> Int# -> Int# -> JSString -> (# Int#, Int# #)
#endif
