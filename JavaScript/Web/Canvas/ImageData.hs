{-# LANGUAGE CPP #-}
{-# LANGUAGE ForeignFunctionInterface, JavaScriptFFI #-}

module JavaScript.Web.Canvas.ImageData ( ImageData
                                       , width
                                       , height
                                       , getData
                                       ) where

import JavaScript.TypedArray

import JavaScript.Web.Canvas.Internal
#if defined(wasm32_HOST_ARCH)
-- the wasm JSFFI only unwraps newtypes whose constructors are in scope
import JavaScript.TypedArray.Internal.Types (SomeTypedArray(..))
#endif

height :: ImageData -> Int
height i = js_height i
{-# INLINE height #-}

width :: ImageData -> Int
width i = js_width i
{-# INLINE width #-}

getData :: ImageData -> Uint8ClampedArray
getData i = js_getData i
{-# INLINE getData #-}

-- -----------------------------------------------------------------------------

#if defined(wasm32_HOST_ARCH)
foreign import javascript unsafe "((x) => { return x.width; })($1)" js_width :: ImageData -> Int
#else
foreign import javascript unsafe
  "((x) => { return x.width; })" js_width :: ImageData -> Int
#endif
#if defined(wasm32_HOST_ARCH)
foreign import javascript unsafe "((x) => { return x.height; })($1)" js_height :: ImageData -> Int
#else
foreign import javascript unsafe
  "((x) => { return x.height; })" js_height :: ImageData -> Int
#endif
#if defined(wasm32_HOST_ARCH)
foreign import javascript unsafe "((x) => { return x.data; })($1)" js_getData :: ImageData -> Uint8ClampedArray
#else
foreign import javascript unsafe
  "((x) => { return x.data; })" js_getData :: ImageData -> Uint8ClampedArray
#endif

