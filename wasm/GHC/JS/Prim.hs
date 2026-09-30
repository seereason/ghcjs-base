{-# LANGUAGE CPP #-}
{-# LANGUAGE MagicHash #-}

{- | The GHC wasm backend's stand-in for the JavaScript backend's
     @GHC.JS.Prim@ (which base only exposes on @arch(javascript)@).

     Built only when compiling for @arch(wasm32)@, so that ghcjs-base and
     the packages built on it (ghcjs-dom-javascript in particular) can
     keep importing @GHC.JS.Prim@ unchanged.  'JSVal' here is the wasm
     backend's 'GHC.Wasm.Prim.JSVal', which JSFFI imports marshal
     directly.
 -}
module GHC.JS.Prim
    ( JSVal
    , JSException(..)
    , WouldBlockException(..)
    , mkJSException
    , fromJSString
    , toJSString
    , toJSArray
    , fromJSArray
    , fromJSInt
    , toJSInt
    , isNull
    , isUndefined
    , jsNull
    , getProp
    , getProp'
    , unsafeGetProp
    , unsafeGetProp'
    , seqList
    ) where

import GHC.Wasm.Prim (JSVal, JSException(..), WouldBlockException(..), JSString(..))
import qualified GHC.Wasm.Prim as W

mkJSException :: JSVal -> IO JSException
mkJSException v = pure (JSException v)

fromJSString :: JSVal -> String
fromJSString v = W.fromJSString (JSString v)
{-# INLINE fromJSString #-}

toJSString :: String -> JSVal
toJSString s = case W.toJSString s of JSString v -> v
{-# INLINE toJSString #-}

toJSArray :: [JSVal] -> IO JSVal
toJSArray xs = do
  a <- js_newArray
  mapM_ (js_push a) xs
  pure a

fromJSArray :: JSVal -> IO [JSVal]
fromJSArray a = do
  n <- js_length a
  mapM (js_index a) [0 .. n - 1]

fromJSInt :: JSVal -> Int
fromJSInt = js_toInt
{-# INLINE fromJSInt #-}

toJSInt :: Int -> JSVal
toJSInt = js_fromInt
{-# INLINE toJSInt #-}

isNull :: JSVal -> Bool
isNull = js_isNull
{-# INLINE isNull #-}

isUndefined :: JSVal -> Bool
isUndefined = js_isUndefined
{-# INLINE isUndefined #-}

jsNull :: JSVal
jsNull = js_null
{-# NOINLINE jsNull #-}

getProp :: JSVal -> String -> IO JSVal
getProp o p = js_getProp o (toJSString p)
{-# INLINE getProp #-}

getProp' :: JSVal -> JSVal -> IO JSVal
getProp' = js_getProp
{-# INLINE getProp' #-}

unsafeGetProp :: JSVal -> String -> IO JSVal
unsafeGetProp = getProp
{-# INLINE unsafeGetProp #-}

unsafeGetProp' :: JSVal -> JSVal -> IO JSVal
unsafeGetProp' = js_getProp
{-# INLINE unsafeGetProp' #-}

-- | Force a list of values (and the list itself) to WHNF.
seqList :: [a] -> [a]
seqList xs = go xs `seq` xs
  where go (y:ys) = y `seq` go ys
        go []     = ()

foreign import javascript unsafe "[]"             js_newArray    :: IO JSVal
foreign import javascript unsafe "$1.push($2)"    js_push        :: JSVal -> JSVal -> IO ()
foreign import javascript unsafe "$1.length"      js_length      :: JSVal -> IO Int
foreign import javascript unsafe "$1[$2]"         js_index       :: JSVal -> Int -> IO JSVal
foreign import javascript unsafe "$1|0"           js_toInt       :: JSVal -> Int
foreign import javascript unsafe "$1"             js_fromInt     :: Int -> JSVal
foreign import javascript unsafe "$1 === null"    js_isNull      :: JSVal -> Bool
foreign import javascript unsafe "$1 === undefined" js_isUndefined :: JSVal -> Bool
foreign import javascript unsafe "null"           js_null        :: JSVal
foreign import javascript unsafe "$1[$2]"         js_getProp     :: JSVal -> JSVal -> IO JSVal
