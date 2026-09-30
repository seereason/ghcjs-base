{-# LANGUAGE CPP #-}
{-# LANGUAGE EmptyDataDecls, ForeignFunctionInterface, JavaScriptFFI,
             OverloadedStrings, DeriveDataTypeable
  #-}

module JavaScript.Web.Canvas ( Context
                             , Canvas
                             , Image
                             , TextAlign(..)
                             , TextBaseline(..)
                             , LineCap(..)
                             , LineJoin(..)
                             , Repeat(..)
                             , Gradient
                             , Pattern
                             , create
                             , unsafeToCanvas
                             , toCanvas
                             , getContext
                             , save
                             , restore
                             , scale
                             , rotate
                             , translate
                             , transform
                             , setTransform
                             , fill
                             , fillRule
                             , stroke
                             , beginPath
                             , closePath
                             , clip
                             , moveTo
                             , lineTo
                             , quadraticCurveTo
                             , bezierCurveTo
                             , arc
                             , arcTo
                             , rect
                             , isPointInPath
                             , fillStyle
                             , strokeStyle
                             , globalAlpha
                             , lineJoin
                             , lineCap
                             , lineWidth
                             , setLineDash
                             , lineDashOffset
                             , miterLimit
                             , fillText
                             , strokeText
                             , font
                             , measureText
                             , textAlign
                             , textBaseline
                             , fillRect
                             , strokeRect
                             , clearRect
                             , drawImage
                             , width
                             , setWidth
                             , height
                             , setHeight
                             ) where

import Prelude hiding (Left, Right)

import Control.Applicative
import Control.Monad

import Data.Data
import Data.Maybe (fromJust)
import Data.Text (Text)
import Data.Typeable

import GHCJS.Foreign
import GHCJS.Marshal
import GHCJS.Types

import           JavaScript.Web.Canvas.Internal

import           JavaScript.Object (Object)
import qualified JavaScript.Object as O
import           JavaScript.Array  (JSArray)
import qualified JavaScript.Array  as A
#if defined(wasm32_HOST_ARCH)
-- the wasm JSFFI only unwraps newtypes whose constructors are in scope
import           Data.JSString.Internal.Type (JSString(..))
import           JavaScript.Object.Internal (Object(..))
import           JavaScript.Array.Internal (SomeJSArray(..))
#endif

data TextAlign = Start
               | End
               | Left
               | Right
               | Center
             deriving (Eq, Show, Enum, Data, Typeable)

data TextBaseline = Top 
                  | Hanging 
                  | Middle
                  | Alphabetic
                  | Ideographic
                  | Bottom
                deriving (Eq, Show, Enum, Data, Typeable)

data LineJoin = LineJoinBevel
              | LineJoinRound
              | LineJoinMiter
            deriving (Eq, Show, Enum)

data LineCap = LineCapButt
             | LineCapRound
             | LineCapSquare deriving (Eq, Show, Enum, Data, Typeable)

data Repeat = Repeat
            | RepeatX
            | RepeatY
            | NoRepeat
            deriving (Eq, Ord, Show, Enum, Data, Typeable)

unsafeToCanvas :: JSVal -> Canvas
unsafeToCanvas r = Canvas r
{-# INLINE unsafeToCanvas #-}

toCanvas :: JSVal -> Maybe Canvas
toCanvas x = error "toCanvas" -- fixme
{-# INLINE toCanvas #-}

create :: Int -> Int -> IO Canvas
create = js_create
{-# INLINE create #-}

getContext :: Canvas -> IO Context
getContext c = js_getContext c
{-# INLINE getContext #-}

save :: Context -> IO ()
save ctx = js_save ctx
{-# INLINE save #-}

restore :: Context -> IO ()
restore = js_restore
{-# INLINE restore #-}

transform :: Double -> Double -> Double -> Double -> Double -> Double -> Context -> IO ()
transform = js_transform
{-# INLINE transform #-}

setTransform :: Double -> Double -> Double -> Double -> Double -> Double -> Context -> IO ()
setTransform = js_setTransform
{-# INLINE setTransform #-}

scale :: Double -> Double -> Context -> IO ()
scale x y ctx = js_scale x y ctx
{-# INLINE scale #-}

translate :: Double -> Double -> Context -> IO ()
translate x y ctx = js_translate x y ctx
{-# INLINE translate #-}

rotate :: Double -> Context -> IO ()
rotate r ctx = js_rotate r ctx
{-# INLINE rotate #-}

fill :: Context -> IO ()
fill ctx = js_fill ctx
{-# INLINE fill #-}

fillRule :: JSString -> Context -> IO ()
fillRule rule ctx = js_fill_rule rule ctx
{-# INLINE fillRule #-}

stroke :: Context -> IO ()
stroke = js_stroke
{-# INLINE stroke #-}

beginPath :: Context -> IO ()
beginPath = js_beginPath
{-# INLINE beginPath #-}

closePath :: Context -> IO ()
closePath = js_closePath
{-# INLINE closePath #-}

clip :: Context -> IO ()
clip = js_clip
{-# INLINE clip #-}

moveTo :: Double -> Double -> Context -> IO ()
moveTo = js_moveTo
{-# INLINE moveTo #-}

lineTo :: Double -> Double -> Context -> IO ()
lineTo = js_lineTo
{-# INLINE lineTo #-}

quadraticCurveTo :: Double -> Double -> Double -> Double -> Context -> IO ()
quadraticCurveTo = js_quadraticCurveTo
{-# INLINE quadraticCurveTo #-}

bezierCurveTo :: Double -> Double -> Double -> Double -> Double -> Double -> Context -> IO ()
bezierCurveTo = js_bezierCurveTo
{-# INLINE bezierCurveTo #-}

arc :: Double -> Double -> Double -> Double -> Double -> Bool -> Context -> IO ()
arc a b c d e bl ctx = js_arc a b c d e bl ctx
{-# INLINE arc #-}

arcTo :: Double -> Double -> Double -> Double -> Double -> Context -> IO ()
arcTo = js_arcTo
{-# INLINE arcTo #-}

rect :: Double -> Double -> Double -> Double -> Context -> IO ()
rect = js_rect
{-# INLINE rect #-}

isPointInPath :: Double -> Double -> Context -> IO ()
isPointInPath = js_isPointInPath
{-# INLINE isPointInPath #-}

fillStyle :: Int -> Int -> Int -> Double -> Context -> IO ()
fillStyle = js_fillStyle
{-# INLINE fillStyle #-}

strokeStyle :: Int -> Int -> Int -> Double -> Context -> IO ()
strokeStyle = js_strokeStyle
{-# INLINE strokeStyle #-}

globalAlpha :: Double -> Context -> IO ()
globalAlpha = js_globalAlpha
{-# INLINE globalAlpha #-}

lineJoin :: LineJoin -> Context -> IO ()
lineJoin LineJoinBevel ctx = js_lineJoin "bevel" ctx
lineJoin LineJoinRound ctx = js_lineJoin "round" ctx
lineJoin LineJoinMiter ctx = js_lineJoin "miter" ctx
{-# INLINE lineJoin #-}

lineCap :: LineCap -> Context -> IO ()
lineCap LineCapButt   ctx = js_lineCap "butt"   ctx
lineCap LineCapRound  ctx = js_lineCap "round"  ctx
lineCap LineCapSquare ctx = js_lineCap "square" ctx
{-# INLINE lineCap #-}

miterLimit :: Double -> Context -> IO ()
miterLimit = js_miterLimit
{-# INLINE miterLimit #-}

-- | pass an array of numbers
setLineDash :: JSArray -> Context -> IO ()
setLineDash arr ctx = js_setLineDash arr ctx
{-# INLINE setLineDash #-}

lineDashOffset :: Double -> Context -> IO ()
lineDashOffset = js_lineDashOffset
{-# INLINE lineDashOffset #-}

textAlign :: TextAlign -> Context -> IO ()
textAlign align ctx = case align of
     Start  -> js_textAlign "start"  ctx
     End    -> js_textAlign "end"    ctx
     Left   -> js_textAlign "left"   ctx
     Right  -> js_textAlign "right"  ctx
     Center -> js_textAlign "center" ctx
{-# INLINE textAlign #-}

textBaseline :: TextBaseline -> Context -> IO ()
textBaseline baseline ctx = case baseline of 
     Top         -> js_textBaseline "top"         ctx
     Hanging     -> js_textBaseline "hanging"     ctx
     Middle      -> js_textBaseline "middle"      ctx
     Alphabetic  -> js_textBaseline "alphabetic"  ctx
     Ideographic -> js_textBaseline "ideographic" ctx
     Bottom      -> js_textBaseline "bottom"      ctx
{-# INLINE textBaseline #-}

lineWidth :: Double -> Context -> IO ()
lineWidth = js_lineWidth
{-# INLINE lineWidth #-}

fillText :: JSString -> Double -> Double -> Context -> IO ()
fillText t x y ctx = js_fillText t x y ctx
{-# INLINE fillText #-}

strokeText :: JSString -> Double -> Double -> Context -> IO ()
strokeText t x y ctx = js_strokeText t x y ctx
{-# INLINE strokeText #-}

font :: JSString -> Context -> IO ()
font f ctx = js_font f ctx
{-# INLINE font #-}

measureText :: JSString -> Context -> IO Double
measureText t ctx = js_measureText t ctx
                    >>= O.getProp "width"
                    >>= liftM fromJust . fromJSVal
{-# INLINE measureText #-}

fillRect :: Double -> Double -> Double -> Double -> Context -> IO ()
fillRect = js_fillRect
{-# INLINE fillRect #-}

clearRect :: Double -> Double -> Double -> Double -> Context -> IO ()
clearRect = js_clearRect
{-# INLINE clearRect #-}

strokeRect :: Double -> Double -> Double -> Double -> Context -> IO ()
strokeRect = js_strokeRect
{-# INLINE strokeRect #-}

drawImage :: Image -> Int -> Int -> Int -> Int -> Context -> IO ()
drawImage = js_drawImage
{-# INLINE drawImage #-}

createPattern :: Image -> Repeat -> Context -> IO Pattern
createPattern img Repeat   ctx = js_createPattern img "repeat"    ctx
createPattern img RepeatX  ctx = js_createPattern img "repeat-x"  ctx
createPattern img RepeatY  ctx = js_createPattern img "repeat-y"  ctx
createPattern img NoRepeat ctx = js_createPattern img "no-repeat" ctx
{-# INLINE createPattern #-}

setWidth :: Int -> Canvas -> IO ()
setWidth w c = js_setWidth w c
{-# INLINE setWidth #-}

width :: Canvas -> IO Int
width c = js_width c
{-# INLINE width #-}

setHeight :: Int -> Canvas -> IO ()
setHeight h c = js_setHeight h c
{-# INLINE setHeight #-}

height :: Canvas -> IO Int
height c = js_height c
{-# INLINE height #-}

-- ----------------------------------------------------------------------------

#if defined(wasm32_HOST_ARCH)
foreign import javascript unsafe "((x,y) => { var r = document.createElement('canvas'); r.width = x; r.height = y; return r; })($1,$2)"
#else
foreign import javascript unsafe
  "((x,y) => { var r = document.createElement('canvas'); r.width = x; r.height = y; return r; })"
#endif
  js_create :: Int -> Int -> IO Canvas
#if defined(wasm32_HOST_ARCH)
foreign import javascript unsafe "((x) => { return x.getContext('2d'); })($1)"
#else
foreign import javascript unsafe "((x) => { return x.getContext('2d'); })"
#endif
  js_getContext :: Canvas -> IO Context
#if defined(wasm32_HOST_ARCH)
foreign import javascript unsafe "((x) => { x.save(); })($1)"
#else
foreign import javascript unsafe "((x) => { x.save(); })"
#endif
  js_save :: Context -> IO ()
#if defined(wasm32_HOST_ARCH)
foreign import javascript unsafe "((x) => { x.restore(); })($1)"
#else
foreign import javascript unsafe "((x) => { x.restore(); })"
#endif
  js_restore  :: Context -> IO ()
#if defined(wasm32_HOST_ARCH)
foreign import javascript unsafe "(($1,$2,$3,$4,$5,$6,$7) => { $7.transform($1,$2,$3,$4,$5,$6); })($1,$2,$3,$4,$5,$6,$7)"
#else
foreign import javascript unsafe "(($1,$2,$3,$4,$5,$6,$7) => { $7.transform($1,$2,$3,$4,$5,$6); })"
#endif
  js_transform :: Double -> Double -> Double -> Double -> Double -> Double -> Context -> IO ()
#if defined(wasm32_HOST_ARCH)
foreign import javascript unsafe "(($1,$2,$3,$4,$5,$6,$7) => { $7.setTransform($1,$2,$3,$4,$5,$6); })($1,$2,$3,$4,$5,$6,$7)"
#else
foreign import javascript unsafe "(($1,$2,$3,$4,$5,$6,$7) => { $7.setTransform($1,$2,$3,$4,$5,$6); })"
#endif
  js_setTransform :: Double -> Double -> Double -> Double -> Double -> Double -> Context -> IO ()
#if defined(wasm32_HOST_ARCH)
foreign import javascript unsafe "((x,y,z) => { z.scale(x,y); })($1,$2,$3)"
#else
foreign import javascript unsafe "((x,y,z) => { z.scale(x,y); })"
#endif
  js_scale :: Double -> Double -> Context -> IO ()
#if defined(wasm32_HOST_ARCH)
foreign import javascript unsafe "((x,y,z) => { z.translate(x,y); })($1,$2,$3)"
#else
foreign import javascript unsafe "((x,y,z) => { z.translate(x,y); })"
#endif
  js_translate  :: Double -> Double -> Context -> IO ()
#if defined(wasm32_HOST_ARCH)
foreign import javascript unsafe "((x,y) => { y.rotate(x); })($1,$2)"
#else
foreign import javascript unsafe "((x,y) => { y.rotate(x); })"
#endif
  js_rotate :: Double -> Context -> IO ()
#if defined(wasm32_HOST_ARCH)
foreign import javascript unsafe "((x) => { x.fill(); })($1)"
#else
foreign import javascript unsafe "((x) => { x.fill(); })"
#endif
  js_fill :: Context -> IO ()
#if defined(wasm32_HOST_ARCH)
foreign import javascript unsafe "((x,y) => { y.fill(x); })($1,$2)"
#else
foreign import javascript unsafe "((x,y) => { y.fill(x); })"
#endif
  js_fill_rule  :: JSString -> Context -> IO ()
#if defined(wasm32_HOST_ARCH)
foreign import javascript unsafe "((x) => { x.stroke(); })($1)"
#else
foreign import javascript unsafe "((x) => { x.stroke(); })"
#endif
  js_stroke :: Context -> IO ()
#if defined(wasm32_HOST_ARCH)
foreign import javascript unsafe "((x) => { x.beginPath(); })($1)"
#else
foreign import javascript unsafe "((x) => { x.beginPath(); })"
#endif
  js_beginPath :: Context -> IO ()
#if defined(wasm32_HOST_ARCH)
foreign import javascript unsafe "((x) => { x.closePath(); })($1)"
#else
foreign import javascript unsafe "((x) => { x.closePath(); })"
#endif
  js_closePath :: Context -> IO ()
#if defined(wasm32_HOST_ARCH)
foreign import javascript unsafe "((x) => { x.clip(); })($1)"
#else
foreign import javascript unsafe "((x) => { x.clip(); })"
#endif
  js_clip  :: Context -> IO ()
#if defined(wasm32_HOST_ARCH)
foreign import javascript unsafe "((x,y,z) => { z.moveTo(x,y); })($1,$2,$3)"
#else
foreign import javascript unsafe "((x,y,z) => { z.moveTo(x,y); })"
#endif
  js_moveTo :: Double -> Double -> Context -> IO ()
#if defined(wasm32_HOST_ARCH)
foreign import javascript unsafe "((x,y,z) => { z.lineTo(x,y); })($1,$2,$3)"
#else
foreign import javascript unsafe "((x,y,z) => { z.lineTo(x,y); })"
#endif
  js_lineTo :: Double -> Double -> Context -> IO ()
#if defined(wasm32_HOST_ARCH)
foreign import javascript unsafe "(($1,$2,$3,$4,$5) => { $5.quadraticCurveTo($1,$2,$3,$4); })($1,$2,$3,$4,$5)"
#else
foreign import javascript unsafe "(($1,$2,$3,$4,$5) => { $5.quadraticCurveTo($1,$2,$3,$4); })"
#endif
  js_quadraticCurveTo :: Double -> Double -> Double -> Double -> Context -> IO ()
#if defined(wasm32_HOST_ARCH)
foreign import javascript unsafe "(($1,$2,$3,$4,$5,$6,$7) => { $7.bezierCurveTo($1,$2,$3,$4,$5,$6); })($1,$2,$3,$4,$5,$6,$7)"
#else
foreign import javascript unsafe "(($1,$2,$3,$4,$5,$6,$7) => { $7.bezierCurveTo($1,$2,$3,$4,$5,$6); })"
#endif
  js_bezierCurveTo :: Double -> Double -> Double -> Double -> Double -> Double -> Context -> IO ()
#if defined(wasm32_HOST_ARCH)
foreign import javascript unsafe "(($1,$2,$3,$4,$5,$6,$7) => { $7.arc($1,$2,$3,$4,$5,$6); })($1,$2,$3,$4,$5,$6,$7)"
#else
foreign import javascript unsafe "(($1,$2,$3,$4,$5,$6,$7) => { $7.arc($1,$2,$3,$4,$5,$6); })"
#endif
  js_arc :: Double -> Double -> Double -> Double -> Double -> Bool -> Context -> IO ()
#if defined(wasm32_HOST_ARCH)
foreign import javascript unsafe "(($1,$2,$3,$4,$5,$6) => { $6.arcTo($1,$2,$3,$4,$5); })($1,$2,$3,$4,$5,$6)"
#else
foreign import javascript unsafe "(($1,$2,$3,$4,$5,$6) => { $6.arcTo($1,$2,$3,$4,$5); })"
#endif
  js_arcTo :: Double -> Double -> Double -> Double -> Double -> Context -> IO ()
#if defined(wasm32_HOST_ARCH)
foreign import javascript unsafe "(($1,$2,$3,$4,$5) => { $5.rect($1,$2,$3,$4); })($1,$2,$3,$4,$5)"
#else
foreign import javascript unsafe "(($1,$2,$3,$4,$5) => { $5.rect($1,$2,$3,$4); })"
#endif
  js_rect :: Double -> Double -> Double -> Double -> Context -> IO ()
#if defined(wasm32_HOST_ARCH)
foreign import javascript unsafe "((x,y,z) => { z.isPointInPath(x,y); })($1,$2,$3)"
#else
foreign import javascript unsafe "((x,y,z) => { z.isPointInPath(x,y); })"
#endif
  js_isPointInPath :: Double -> Double -> Context -> IO ()
#if defined(wasm32_HOST_ARCH)
foreign import javascript unsafe "(($1,$2,$3,$4,$5) => { $5.fillStyle = 'rgba(' + $1 + ',' + $2 + ',' + $3 + ',' + $4 + ')'; })($1,$2,$3,$4,$5)"
#else
foreign import javascript unsafe
  "(($1,$2,$3,$4,$5) => { $5.fillStyle = 'rgba(' + $1 + ',' + $2 + ',' + $3 + ',' + $4 + ')'; })"
#endif
  js_fillStyle :: Int -> Int -> Int -> Double -> Context -> IO ()
#if defined(wasm32_HOST_ARCH)
foreign import javascript unsafe "(($1,$2,$3,$4,$5) => { $5.strokeStyle = 'rgba(' + $1 + ',' + $2 + ',' + $3 + ',' + $4 + ')'; })($1,$2,$3,$4,$5)"
#else
foreign import javascript unsafe
  "(($1,$2,$3,$4,$5) => { $5.strokeStyle = 'rgba(' + $1 + ',' + $2 + ',' + $3 + ',' + $4 + ')'; })"
#endif
  js_strokeStyle :: Int -> Int -> Int -> Double -> Context -> IO ()
#if defined(wasm32_HOST_ARCH)
foreign import javascript unsafe "((x,y) => { y.globalAlpha = x; })($1,$2)"
#else
foreign import javascript unsafe "((x,y) => { y.globalAlpha = x; })"
#endif
  js_globalAlpha :: Double           -> Context -> IO ()
#if defined(wasm32_HOST_ARCH)
foreign import javascript unsafe "((x,y) => { y.lineJoin = x; })($1,$2)"
#else
foreign import javascript unsafe
  "((x,y) => { y.lineJoin = x; })"
#endif
  js_lineJoin :: JSString -> Context -> IO ()
#if defined(wasm32_HOST_ARCH)
foreign import javascript unsafe "((x,y) => { y.lineCap = x; })($1,$2)"
#else
foreign import javascript unsafe "((x,y) => { y.lineCap = x; })"
#endif
  js_lineCap :: JSString -> Context -> IO ()
#if defined(wasm32_HOST_ARCH)
foreign import javascript unsafe "((x,y) => { y.miterLimit = x; })($1,$2)"
#else
foreign import javascript unsafe "((x,y) => { y.miterLimit = x; })"
#endif
  js_miterLimit :: Double -> Context -> IO ()
#if defined(wasm32_HOST_ARCH)
foreign import javascript unsafe "((x,y) => { y.setLineDash(x); })($1,$2)"
#else
foreign import javascript unsafe "((x,y) => { y.setLineDash(x); })"
#endif
  js_setLineDash :: JSArray -> Context -> IO ()
#if defined(wasm32_HOST_ARCH)
foreign import javascript unsafe "((x,y) => { y.lineDashOffset = x; })($1,$2)"
#else
foreign import javascript unsafe "((x,y) => { y.lineDashOffset = x; })"
#endif
  js_lineDashOffset :: Double -> Context -> IO ()
#if defined(wasm32_HOST_ARCH)
foreign import javascript unsafe "((x,y) => { y.font = x; })($1,$2)"
#else
foreign import javascript unsafe "((x,y) => { y.font = x; })"
#endif
  js_font :: JSString -> Context -> IO ()
#if defined(wasm32_HOST_ARCH)
foreign import javascript unsafe "((x,y) => { y.textAlign = x; })($1,$2)"
#else
foreign import javascript unsafe "((x,y) => { y.textAlign = x; })"
#endif
  js_textAlign :: JSString -> Context -> IO ()
#if defined(wasm32_HOST_ARCH)
foreign import javascript unsafe "((x,y) => { y.textBaseline = x; })($1,$2)"
#else
foreign import javascript unsafe "((x,y) => { y.textBaseline = x; })"
#endif
  js_textBaseline :: JSString -> Context -> IO ()
#if defined(wasm32_HOST_ARCH)
foreign import javascript unsafe "((x,y) => { y.lineWidth = x; })($1,$2)"
#else
foreign import javascript unsafe "((x,y) => { y.lineWidth = x; })"
#endif
  js_lineWidth :: Double -> Context -> IO ()
#if defined(wasm32_HOST_ARCH)
foreign import javascript unsafe "(($1,$2,$3,$4) => { $4.fillText($1,$2,$3); })($1,$2,$3,$4)"
#else
foreign import javascript unsafe "(($1,$2,$3,$4) => { $4.fillText($1,$2,$3); })"
#endif
  js_fillText :: JSString -> Double -> Double -> Context -> IO ()
#if defined(wasm32_HOST_ARCH)
foreign import javascript unsafe "(($1,$2,$3,$4) => { $4.strokeText($1,$2,$3); })($1,$2,$3,$4)"
#else
foreign import javascript unsafe "(($1,$2,$3,$4) => { $4.strokeText($1,$2,$3); })"
#endif
  js_strokeText :: JSString -> Double -> Double -> Context -> IO ()
#if defined(wasm32_HOST_ARCH)
foreign import javascript unsafe "((x,y) => { return y.measureText(x); })($1,$2)"
#else
foreign import javascript unsafe "((x,y) => { return y.measureText(x); })"
#endif
  js_measureText :: JSString                    -> Context -> IO Object
#if defined(wasm32_HOST_ARCH)
foreign import javascript unsafe "(($1,$2,$3,$4,$5) => { $5.fillRect($1,$2,$3,$4); })($1,$2,$3,$4,$5)"
#else
foreign import javascript unsafe "(($1,$2,$3,$4,$5) => { $5.fillRect($1,$2,$3,$4); })"
#endif
  js_fillRect :: Double -> Double -> Double -> Double -> Context -> IO ()
#if defined(wasm32_HOST_ARCH)
foreign import javascript unsafe "(($1,$2,$3,$4,$5) => { $5.clearRect($1,$2,$3,$4); })($1,$2,$3,$4,$5)"
#else
foreign import javascript unsafe "(($1,$2,$3,$4,$5) => { $5.clearRect($1,$2,$3,$4); })"
#endif
  js_clearRect :: Double -> Double -> Double -> Double -> Context -> IO ()
#if defined(wasm32_HOST_ARCH)
foreign import javascript unsafe "(($1,$2,$3,$4,$5) => { $5.strokeRect($1,$2,$3,$4); })($1,$2,$3,$4,$5)"
#else
foreign import javascript unsafe "(($1,$2,$3,$4,$5) => { $5.strokeRect($1,$2,$3,$4); })"
#endif
  js_strokeRect :: Double -> Double -> Double -> Double -> Context -> IO ()
#if defined(wasm32_HOST_ARCH)
foreign import javascript unsafe "(($1,$2,$3,$4,$5,$6) => { $6.drawImage($1,$2,$3,$4,$5); })($1,$2,$3,$4,$5,$6)"
#else
foreign import javascript unsafe "(($1,$2,$3,$4,$5,$6) => { $6.drawImage($1,$2,$3,$4,$5); })"
#endif
  js_drawImage :: Image -> Int -> Int -> Int -> Int -> Context -> IO () 
#if defined(wasm32_HOST_ARCH)
foreign import javascript unsafe "((x,y,z) => { return z.createPattern(x,y); })($1,$2,$3)"
#else
foreign import javascript unsafe "((x,y,z) => { return z.createPattern(x,y); })"
#endif
  js_createPattern :: Image -> JSString -> Context -> IO Pattern
#if defined(wasm32_HOST_ARCH)
foreign import javascript unsafe "((x) => { return x.width; })($1)"
#else
foreign import javascript unsafe "((x) => { return x.width; })"
#endif
  js_width :: Canvas -> IO Int
#if defined(wasm32_HOST_ARCH)
foreign import javascript unsafe "((x) => { return x.height; })($1)"
#else
foreign import javascript unsafe "((x) => { return x.height; })"
#endif
  js_height :: Canvas -> IO Int
#if defined(wasm32_HOST_ARCH)
foreign import javascript unsafe "((x,y) => { return y.width = x; })($1,$2)"
#else
foreign import javascript unsafe "((x,y) => { return y.width = x; })"
#endif
  js_setWidth :: Int -> Canvas -> IO ()
#if defined(wasm32_HOST_ARCH)
foreign import javascript unsafe "((x,y) => { return y.height = x; })($1,$2)"
#else
foreign import javascript unsafe "((x,y) => { return y.height = x; })"
#endif
  js_setHeight :: Int -> Canvas -> IO ()
