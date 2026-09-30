{-# LANGUAGE CPP #-}
{-# LANGUAGE ForeignFunctionInterface, JavaScriptFFI #-}

module JavaScript.Web.Canvas.TextMetrics ( width
                                         , actualBoundingBoxLeft
                                         , actualBoundingBoxRight
                                         , fontBoundingBoxAscent
                                         , fontBoundingBoxDescent
                                         , actualBoundingBoxAscent
                                         , actualBoundingBoxDescent
                                         , emHeightAscent
                                         , emHeightDescent
                                         , hangingBaseline
                                         , alphabeticBaseline
                                         , ideographicBaseline
                                         ) where

import JavaScript.Web.Canvas.Internal

width :: TextMetrics -> Double
width tm = js_width tm
{-# INLINE width #-}

actualBoundingBoxLeft :: TextMetrics -> Double
actualBoundingBoxLeft tm = js_actualBoundingBoxLeft tm
{-# INLINE actualBoundingBoxLeft #-}

actualBoundingBoxRight :: TextMetrics -> Double
actualBoundingBoxRight tm = js_actualBoundingBoxRight tm
{-# INLINE actualBoundingBoxRight #-}

fontBoundingBoxAscent :: TextMetrics -> Double
fontBoundingBoxAscent tm = js_fontBoundingBoxAscent tm
{-# INLINE fontBoundingBoxAscent #-}

fontBoundingBoxDescent :: TextMetrics -> Double
fontBoundingBoxDescent tm = js_fontBoundingBoxDescent tm
{-# INLINE fontBoundingBoxDescent #-}

actualBoundingBoxAscent :: TextMetrics -> Double
actualBoundingBoxAscent tm = js_actualBoundingBoxAscent tm
{-# INLINE actualBoundingBoxAscent #-}

actualBoundingBoxDescent :: TextMetrics -> Double
actualBoundingBoxDescent tm = js_actualBoundingBoxDescent tm
{-# INLINE actualBoundingBoxDescent #-}

emHeightAscent :: TextMetrics -> Double
emHeightAscent tm = js_emHeightAscent tm
{-# INLINE emHeightAscent #-}

emHeightDescent :: TextMetrics -> Double
emHeightDescent tm = js_emHeightDescent tm
{-# INLINE emHeightDescent #-}

hangingBaseline :: TextMetrics -> Double
hangingBaseline tm = js_hangingBaseline tm
{-# INLINE hangingBaseline #-}

alphabeticBaseline :: TextMetrics -> Double
alphabeticBaseline tm = js_alphabeticBaseline tm
{-# INLINE alphabeticBaseline #-}

ideographicBaseline :: TextMetrics -> Double
ideographicBaseline tm = js_ideographicBaseline tm
{-# INLINE ideographicBaseline #-}

-- -----------------------------------------------------------------------------

#if defined(wasm32_HOST_ARCH)
foreign import javascript unsafe "((x) => { return x.width; })($1)" js_width :: TextMetrics -> Double
#else
foreign import javascript unsafe
  "((x) => { return x.width; })" js_width :: TextMetrics -> Double
#endif
#if defined(wasm32_HOST_ARCH)
foreign import javascript unsafe "((x) => { return x.actualBoundingBoxLeft; })($1)" js_actualBoundingBoxLeft :: TextMetrics -> Double
#else
foreign import javascript unsafe
  "((x) => { return x.actualBoundingBoxLeft; })" js_actualBoundingBoxLeft :: TextMetrics -> Double
#endif
#if defined(wasm32_HOST_ARCH)
foreign import javascript unsafe "((x) => { return x.actualBoundingBoxRight; })($1)" js_actualBoundingBoxRight :: TextMetrics -> Double
#else
foreign import javascript unsafe
  "((x) => { return x.actualBoundingBoxRight; })" js_actualBoundingBoxRight :: TextMetrics -> Double
#endif
#if defined(wasm32_HOST_ARCH)
foreign import javascript unsafe "((x) => { return x.fontBoundingBoxAscent; })($1)" js_fontBoundingBoxAscent :: TextMetrics -> Double
#else
foreign import javascript unsafe
  "((x) => { return x.fontBoundingBoxAscent; })" js_fontBoundingBoxAscent :: TextMetrics -> Double
#endif
#if defined(wasm32_HOST_ARCH)
foreign import javascript unsafe "((x) => { return x.fontBoundingBoxDescent; })($1)" js_fontBoundingBoxDescent :: TextMetrics -> Double
#else
foreign import javascript unsafe
  "((x) => { return x.fontBoundingBoxDescent; })" js_fontBoundingBoxDescent :: TextMetrics -> Double
#endif
#if defined(wasm32_HOST_ARCH)
foreign import javascript unsafe "((x) => { return x.actualBoundingBoxAscent; })($1)" js_actualBoundingBoxAscent :: TextMetrics -> Double
#else
foreign import javascript unsafe
  "((x) => { return x.actualBoundingBoxAscent; })" js_actualBoundingBoxAscent :: TextMetrics -> Double
#endif
#if defined(wasm32_HOST_ARCH)
foreign import javascript unsafe "((x) => { return x.actualBoundingBoxDescent; })($1)" js_actualBoundingBoxDescent :: TextMetrics -> Double
#else
foreign import javascript unsafe
  "((x) => { return x.actualBoundingBoxDescent; })" js_actualBoundingBoxDescent :: TextMetrics -> Double
#endif
#if defined(wasm32_HOST_ARCH)
foreign import javascript unsafe "((x) => { return x.emHeightAscent; })($1)" js_emHeightAscent :: TextMetrics -> Double
#else
foreign import javascript unsafe
  "((x) => { return x.emHeightAscent; })" js_emHeightAscent :: TextMetrics -> Double
#endif
#if defined(wasm32_HOST_ARCH)
foreign import javascript unsafe "((x) => { return x.emHeightDescent; })($1)" js_emHeightDescent :: TextMetrics -> Double
#else
foreign import javascript unsafe
  "((x) => { return x.emHeightDescent; })" js_emHeightDescent :: TextMetrics -> Double
#endif
#if defined(wasm32_HOST_ARCH)
foreign import javascript unsafe "((x) => { return x.hangingBaseline; })($1)" js_hangingBaseline :: TextMetrics -> Double
#else
foreign import javascript unsafe
  "((x) => { return x.hangingBaseline; })" js_hangingBaseline :: TextMetrics -> Double
#endif
#if defined(wasm32_HOST_ARCH)
foreign import javascript unsafe "((x) => { return x.alphabeticBaseline; })($1)" js_alphabeticBaseline :: TextMetrics -> Double
#else
foreign import javascript unsafe
  "((x) => { return x.alphabeticBaseline; })" js_alphabeticBaseline :: TextMetrics -> Double
#endif
#if defined(wasm32_HOST_ARCH)
foreign import javascript unsafe "((x) => { return x.ideographicBaseline; })($1)" js_ideographicBaseline :: TextMetrics -> Double
#else
foreign import javascript unsafe
  "((x) => { return x.ideographicBaseline; })" js_ideographicBaseline :: TextMetrics -> Double
#endif


