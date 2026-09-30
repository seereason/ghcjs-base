{-# LANGUAGE CPP #-}
{-# LANGUAGE ForeignFunctionInterface, JavaScriptFFI #-}

module JavaScript.Web.CloseEvent ( CloseEvent
                                 , getCode
                                 , getReason
                                 , wasClean
                                 ) where

import Data.JSString

import JavaScript.Web.CloseEvent.Internal
#if defined(wasm32_HOST_ARCH)
-- the wasm JSFFI only unwraps newtypes whose constructors are in scope
import Data.JSString.Internal.Type (JSString(..))
#endif

getCode :: CloseEvent -> Int
getCode c = js_getCode c
{-# INLINE getCode #-}

getReason :: CloseEvent -> JSString
getReason c = js_getReason c
{-# INLINE getReason #-}

wasClean :: CloseEvent -> Bool
wasClean c = js_wasClean c
{-# INLINE wasClean #-}

-- -----------------------------------------------------------------------------

#if defined(wasm32_HOST_ARCH)
foreign import javascript unsafe "((x) => { return x.code; })($1)"     js_getCode   :: CloseEvent -> Int
#else
foreign import javascript unsafe
  "((x) => { return x.code; })"     js_getCode   :: CloseEvent -> Int
#endif
#if defined(wasm32_HOST_ARCH)
foreign import javascript unsafe "((x) => { return x.reason; })($1)"   js_getReason :: CloseEvent -> JSString
#else
foreign import javascript unsafe
  "((x) => { return x.reason; })"   js_getReason :: CloseEvent -> JSString
#endif
#if defined(wasm32_HOST_ARCH)
foreign import javascript unsafe "((x) => { return x.wasClean; })($1)" js_wasClean  :: CloseEvent -> Bool
#else
foreign import javascript unsafe
  "((x) => { return x.wasClean; })" js_wasClean  :: CloseEvent -> Bool
#endif
