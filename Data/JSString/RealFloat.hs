{-# LANGUAGE CPP #-}
{-# LANGUAGE ForeignFunctionInterface, JavaScriptFFI, MagicHash,
             UnliftedFFITypes
  #-}
module Data.JSString.RealFloat ( FPFormat(..)
                               , realFloat
                               , formatRealFloat
                               , formatDouble
                               , formatFloat
                               ) where

import GHC.Exts (Int#, Float#, Double#, Int(..), Float(..), Double(..))

import Data.JSString
#if defined(wasm32_HOST_ARCH)
import Data.JSString.Internal.Type (JSString(..))
import GHC.Float (float2Double)
#endif

-- | Control the rendering of floating point numbers.
data FPFormat = Exponent
              -- ^ Scientific notation (e.g. @2.3e123@).
              | Fixed
              -- ^ Standard decimal notation.
              | Generic
              -- ^ Use decimal notation for values between @0.1@ and
              -- @9,999,999@, and scientific notation otherwise.
                deriving (Enum, Read, Show)

realFloat :: (RealFloat a) => a -> JSString
realFloat = error "Data.JSString.RealFloat.realFloat not yet implemented"
{-# RULES "realFloat/Double" realFloat = genericDouble #-}
{-# RULES "realFoat/Float"   realFloat = genericFloat  #-}
{-# NOINLINE realFloat #-}

formatRealFloat :: (RealFloat a)
                => FPFormat
                -> Maybe Int
                -> a
                -> JSString
formatRealFloat = error "Data.JSString.RealFloat.formatRealFloat not yet implemented"
{-# RULES "formatRealFloat/Double" formatRealFloat = formatDouble #-}
{-# RULES "formatRealFloat/Float"  formatRealFloat = formatFloat  #-}
{-# NOINLINE formatRealFloat #-}

genericDouble :: Double -> JSString
genericDouble (D# d) = js_doubleGeneric -1# d
{-# INLINE genericDouble #-}

genericFloat :: Float -> JSString
genericFloat (F# f) = js_floatGeneric -1# f
{-# INLINE genericFloat #-}

formatDouble :: FPFormat -> Maybe Int -> Double -> JSString
formatDouble fmt Nothing (D# d)
  = case fmt of
     Fixed    -> js_doubleToFixed -1# d
     Exponent -> js_doubleToExponent -1# d
     Generic  -> js_doubleGeneric -1# d
formatDouble fmt (Just (I# decs)) (D# d)
  = case fmt of
      Fixed    -> js_doubleToFixed decs d
      Exponent -> js_doubleToExponent decs d
      Generic  -> js_doubleGeneric decs d
{-# INLINE formatDouble #-}

formatFloat :: FPFormat -> Maybe Int -> Float -> JSString
formatFloat fmt Nothing (F# f)
  = case fmt of
     Fixed    -> js_floatToFixed -1# f
     Exponent -> js_floatToExponent -1# f
     Generic  -> js_floatGeneric -1# f
formatFloat fmt (Just (I# decs)) (F# f)
  = case fmt of
      Fixed    -> js_floatToFixed decs f
      Exponent -> js_floatToExponent decs f
      Generic  -> js_floatGeneric decs f
{-# INLINE formatFloat #-}


#if defined(wasm32_HOST_ARCH)
-- h$jsstringDoubleToFixed, h$jsstringDoubleToExponent,
-- h$jsstringDoubleGeneric and h$jsstringZeroes (jsbits/jsstring.js)
-- inlined.  $1: number of decimals (-1: unspecified), $2: the number.
-- Float values are converted to Double (exact) as on the JS backend,
-- where a Float# is a JavaScript number.
foreign import javascript unsafe
  "var decs = $1, d = $2; var z = function(n) { var r; switch(n&7) { case 0: r = ''; break; case 1: r = '0'; break; case 2: r = '00'; break; case 3: r = '000'; break; case 4: r = '0000'; break; case 5: r = '00000'; break; case 6: r = '000000'; break; case 7: r = '0000000'; } for(var i=n>>3;i>0;i--) r = r + '00000000'; return r; }; if(decs >= 0) { if(Math.abs(d) < 1e21) { var r = d.toFixed(Math.min(20,decs)); if(decs > 20) r = r + z(decs-20); return r; } else { var r = d.toExponential(); var ei = r.indexOf('e'); var di = r.indexOf('.'); var e = parseInt(r.substr(ei+1)); return r.substring(0,di) + r.substring(di,ei) + z(di-ei+e) + ((decs > 0) ? ('.' + z(decs)) : ''); } } var r = Math.abs(d).toExponential(); var ei = r.indexOf('e'); var e = parseInt(r.substr(ei+1)); var m = d < 0 ? '-' : ''; r = r.substr(0,1) + r.substring(2,ei); if(e >= 0) { return (e > r.length) ? m + r + z(r.length-e-1) + '.0' : m + r.substr(0,e+1) + '.' + r.substr(e+1); } else { return m + '0.' + z(-e-1) + r; }"
  js_doubleToFixed_wasm :: Int -> Double -> JSString
js_doubleToFixed :: Int# -> Double# -> JSString
js_doubleToFixed a1 a2 = js_doubleToFixed_wasm (I# a1) (D# a2)
{-# INLINE js_doubleToFixed #-}
js_floatToFixed :: Int# -> Float# -> JSString
js_floatToFixed a1 a2 = js_doubleToFixed_wasm (I# a1) (float2Double (F# a2))
{-# INLINE js_floatToFixed #-}

foreign import javascript unsafe
  "var decs = $1, d = $2; var z = function(n) { var r; switch(n&7) { case 0: r = ''; break; case 1: r = '0'; break; case 2: r = '00'; break; case 3: r = '000'; break; case 4: r = '0000'; break; case 5: r = '00000'; break; case 6: r = '000000'; break; case 7: r = '0000000'; } for(var i=n>>3;i>0;i--) r = r + '00000000'; return r; }; var r; if(decs === -1) { r = d.toExponential().replace('+',''); } else { r = d.toExponential(Math.max(1, Math.min(20,decs))).replace('+',''); } if(r.indexOf('.') === -1) { r = r.replace('e', '.0e'); } if(decs > 20) r = r.replace('e', z(decs-20)+'e'); return r;"
  js_doubleToExponent_wasm :: Int -> Double -> JSString
js_doubleToExponent :: Int# -> Double# -> JSString
js_doubleToExponent a1 a2 = js_doubleToExponent_wasm (I# a1) (D# a2)
{-# INLINE js_doubleToExponent #-}
js_floatToExponent :: Int# -> Float# -> JSString
js_floatToExponent a1 a2 = js_doubleToExponent_wasm (I# a1) (float2Double (F# a2))
{-# INLINE js_floatToExponent #-}
foreign import javascript unsafe
  "var decs = $1, d = $2; var r; if(decs === -1) { r = d.toString(10).replace('+',''); } else { r = d.toPrecision(Math.max(decs+1,1)).replace('+',''); } if(decs !== 0 && r.indexOf('.') === -1) { if(r.indexOf('e') !== -1) { r = r.replace('e', '.0e'); } else { r = r + '.0'; } } return r;"
  js_doubleGeneric_wasm :: Int -> Double -> JSString
js_doubleGeneric :: Int# -> Double# -> JSString
js_doubleGeneric a1 a2 = js_doubleGeneric_wasm (I# a1) (D# a2)
{-# INLINE js_doubleGeneric #-}
js_floatGeneric :: Int# -> Float# -> JSString
js_floatGeneric a1 a2 = js_doubleGeneric_wasm (I# a1) (float2Double (F# a2))
{-# INLINE js_floatGeneric #-}
#else
foreign import javascript unsafe
  "h$jsstringDoubleToFixed"
  js_doubleToFixed :: Int# -> Double# -> JSString
foreign import javascript unsafe
  "h$jsstringDoubleToFixed"
  js_floatToFixed :: Int# -> Float# -> JSString

foreign import javascript unsafe
  "h$jsstringDoubleToExponent"
  js_doubleToExponent :: Int# -> Double# -> JSString
foreign import javascript unsafe
  "h$jsstringDoubleToExponent"
  js_floatToExponent :: Int# -> Float# -> JSString
foreign import javascript unsafe
  "h$jsstringDoubleGeneric"
  js_doubleGeneric :: Int# -> Double# -> JSString
foreign import javascript unsafe
  "h$jsstringDoubleGeneric"
  js_floatGeneric :: Int# -> Float# -> JSString
                        
#endif
