{-# LANGUAGE CPP #-}
{-# LANGUAGE JavaScriptFFI #-}
{-# LANGUAGE ForeignFunctionInterface #-}
{-# LANGUAGE GHCForeignImportPrim #-}
{-# LANGUAGE UnliftedFFITypes #-}
{-# LANGUAGE UnboxedTuples #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE MagicHash #-}

module Data.JSString.RegExp ( RegExp
                            , pattern
                            , isMultiline
                            , isIgnoreCase
                            , Match(..)
                            , REFlags(..)
                            , create
                            , test
                            , exec
                            , execNext
                            ) where

import GHC.JS.Prim
import GHC.Exts (Any, Int#, Int(..))

import Unsafe.Coerce (unsafeCoerce)

import Data.JSString
import Data.Typeable
#if defined(wasm32_HOST_ARCH)
import Data.JSString.Internal.Type (JSString(..))
#endif

newtype RegExp = RegExp JSVal deriving Typeable

data REFlags = REFlags { multiline  :: !Bool
                       , ignoreCase :: !Bool
                       }

data Match = Match { matched       :: !JSString  -- ^ the matched string
                   , subMatched    :: [JSString] -- ^ the matched parentesized substrings
                   , matchRawIndex :: !Int       -- ^ the raw index of the match in the string
                   , matchInput    :: !JSString  -- ^ the input string
                   }

create :: REFlags -> JSString -> RegExp
create flags pat = js_createRE pat flags'
  where
    flags' | multiline flags = if ignoreCase flags then "mi" else "m"
           | otherwise       = if ignoreCase flags then "i"  else ""
{-# INLINE create #-}

pattern :: RegExp -> JSString
pattern re = js_pattern re

isMultiline :: RegExp -> Bool
isMultiline re = js_isMultiline re

isIgnoreCase :: RegExp -> Bool
isIgnoreCase re = js_isIgnoreCase re

test :: JSString -> RegExp -> Bool
test x re = js_test x re
{-# INLINE test #-}

exec :: JSString -> RegExp -> Maybe Match
exec x re = exec' 0# x re
{-# INLINE exec #-}

execNext :: Match -> RegExp -> Maybe Match
execNext m re = case matchRawIndex m of
                  I# i -> exec' i (matchInput m) re
{-# INLINE execNext #-}

exec' :: Int# -> JSString -> RegExp -> Maybe Match
exec' i x re = case js_exec i x re of
                 (# -1#, _, _ #) -> Nothing
                 (# i',  y, z #) -> Just (Match y (unsafeCoerce z) (I# i) x)
{-# INLINE exec' #-}

matches :: JSString -> RegExp -> [Match]
matches x r = maybe [] go (exec x r)
  where
    go m = m : maybe [] go (execNext m r)
{-# INLINE matches #-}

replace :: RegExp -> JSString -> JSString -> JSString
replace x r = error "Data.JSString.RegExp.replace not implemented"
{-# INLINE replace #-}

split :: JSString -> RegExp -> [JSString]
split x r = unsafeCoerce (js_split -1# x r)
{-# INLINE split #-}

splitN :: Int -> JSString -> RegExp -> [JSString]
splitN (I# k) x r = unsafeCoerce (js_split k x r)
{-# INLINE splitN #-}

-- ----------------------------------------------------------------------------

#if defined(wasm32_HOST_ARCH)
foreign import javascript unsafe "((x,y) => { return new RegExp(x,y); })($1,$2)" js_createRE :: JSString -> JSString -> RegExp
#else
foreign import javascript unsafe
  "((x,y) => { return new RegExp(x,y); })" js_createRE :: JSString -> JSString -> RegExp
#endif
#if defined(wasm32_HOST_ARCH)
foreign import javascript unsafe "((x,y) => { return y.test(x); })($1,$2)" js_test :: JSString -> RegExp -> Bool
#else
foreign import javascript unsafe
  "((x,y) => { return y.test(x); })" js_test :: JSString -> RegExp -> Bool
#endif
#if defined(wasm32_HOST_ARCH)
-- h$jsstringExecRE inlined; returns [index, match, [submatches]], with
-- index -1 for no match.
foreign import javascript unsafe
  "var re = $3; re.lastIndex = $1; var m = re.exec($2); if(m === null) return [-1, '', []]; var a = [], x, j = 1; while(true) { x = m[j]; if(typeof x === 'undefined') break; a[j-1] = x; j++; } return [m.index, m[0], a];"
  js_exec_wasm :: Int -> JSString -> RegExp -> JSVal
foreign import javascript unsafe "$1.length" js_reArrLen :: JSVal -> Int
foreign import javascript unsafe "$1[$2]"    js_reArrVal :: JSVal -> Int -> JSVal
foreign import javascript unsafe "$1[$2]"    js_reArrInt :: JSVal -> Int -> Int
js_reArrToList :: JSVal -> [JSString]
js_reArrToList a = go 0
  where n = js_reArrLen a
        go i | i >= n    = []
             | otherwise = JSString (js_reArrVal a i) : go (i + 1)
js_exec :: Int# -> JSString -> RegExp -> (# Int#, JSString, Any {- [JSString] -} #)
js_exec i x re =
  let r = js_exec_wasm (I# i) x re
  in  case js_reArrInt r 0 of
        I# n -> (# n, JSString (js_reArrVal r 1)
                 , unsafeCoerce (js_reArrToList (js_reArrVal r 2)) #)
foreign import javascript unsafe
  "$3.replace($1, $2)" js_replace :: RegExp -> JSString -> JSString -> JSString
-- h$jsstringSplitRE is broken in jsbits (it refers to undefined variables
-- and swaps the string and RegExp arguments); this implements the
-- intended behaviour: split the string $2 on the RegExp $3, with limit $1
-- (no limit if negative).
foreign import javascript unsafe
  "return ($1 < 0) ? $2.split($3) : $2.split($3, $1);"
  js_split_wasm :: Int -> JSString -> RegExp -> JSVal
js_split :: Int# -> JSString -> RegExp -> Any -- [JSString]
js_split k x re = unsafeCoerce (js_reArrToList (js_split_wasm (I# k) x re))
#else
foreign import javascript unsafe
  "h$jsstringExecRE" js_exec
  :: Int# -> JSString -> RegExp -> (# Int#, JSString, Any {- [JSString] -} #)
foreign import javascript unsafe
  "h$jsstringReplaceRE" js_replace :: RegExp -> JSString -> JSString -> JSString
foreign import javascript unsafe
  "h$jsstringSplitRE" js_split :: Int# -> JSString -> RegExp -> Any -- [JSString]
#endif
#if defined(wasm32_HOST_ARCH)
foreign import javascript unsafe "((x) => { return x.multiline; })($1)" js_isMultiline :: RegExp -> Bool
#else
foreign import javascript unsafe
  "((x) => { return x.multiline; })" js_isMultiline :: RegExp -> Bool
#endif
#if defined(wasm32_HOST_ARCH)
foreign import javascript unsafe "((x) => { return x.ignoreCase; })($1)" js_isIgnoreCase :: RegExp -> Bool
#else
foreign import javascript unsafe
  "((x) => { return x.ignoreCase; })" js_isIgnoreCase :: RegExp -> Bool
#endif
#if defined(wasm32_HOST_ARCH)
foreign import javascript unsafe "((x) => { return x.pattern; })($1)" js_pattern :: RegExp -> JSString
#else
foreign import javascript unsafe
  "((x) => { return x.pattern; })" js_pattern :: RegExp -> JSString
#endif

