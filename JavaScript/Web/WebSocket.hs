{-# LANGUAGE CPP #-}
{-# LANGUAGE ForeignFunctionInterface #-}
{-# LANGUAGE JavaScriptFFI #-}
{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE InterruptibleFFI #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE MagicHash #-}
{-# LANGUAGE UnliftedFFITypes #-}
{-# LANGUAGE GHCForeignImportPrim #-}
{-# LANGUAGE UnboxedTuples #-}

module JavaScript.Web.WebSocket ( WebSocket
                                , WebSocketRequest(..)
                                , ReadyState(..)
                                , BinaryType(..)
                                , connect
                                , close
                                , send
                                , sendArrayBuffer
                                , sendBlob
                                , getBufferedAmount
                                , getExtensions
                                , getProtocol
                                , getReadyState
                                , getBinaryType
                                , setBinaryType
                                , getUrl
                                ) where

import           GHCJS.Concurrent
import           GHCJS.Types
import           GHC.JS.Prim
import           GHC.JS.Foreign.Callback (Callback)
import qualified GHC.JS.Foreign.Callback          as CB

import           GHC.Exts

import           Control.Exception
import           Control.Monad

import           Data.Data
import           Data.Maybe
import           Data.Typeable

import           System.IO

import           Data.JSString (JSString)
import qualified Data.JSString as JSS

import           JavaScript.Array (JSArray)
import qualified JavaScript.Array as JSA
import           JavaScript.TypedArray.ArrayBuffer (ArrayBuffer)
import           JavaScript.Web.Blob (Blob)
import           JavaScript.Web.MessageEvent
import           JavaScript.Web.MessageEvent.Internal
import           JavaScript.Web.CloseEvent
import           JavaScript.Web.CloseEvent.Internal
import           JavaScript.Web.ErrorEvent
import           JavaScript.Web.ErrorEvent.Internal
#if defined(wasm32_HOST_ARCH)
-- the wasm JSFFI only unwraps newtypes whose constructors are in scope
import           Data.JSString.Internal.Type (JSString(..))
import           JavaScript.Array.Internal (SomeJSArray(..))
import           JavaScript.TypedArray.ArrayBuffer.Internal (SomeArrayBuffer(..))
import           JavaScript.Web.Blob.Internal (SomeBlob(..))
#endif

import Unsafe.Coerce

data WebSocketRequest = WebSocketRequest
  { url       :: JSString
  , protocols :: [JSString]
  , onClose   :: Maybe (CloseEvent -> IO ()) -- ^ called when the connection closes (at most once)
  , onMessage :: Maybe (MessageEvent -> IO ()) -- ^ called for each message
  }

newtype WebSocket = WebSocket JSVal
-- instance IsJSVal WebSocket

data ReadyState = Connecting | Open | Closing | Closed
  deriving (Data, Typeable, Enum, Eq, Ord, Show)

data BinaryType = Blob | ArrayBuffer
  deriving (Data, Typeable, Enum, Eq, Ord, Show)

{- | create a WebSocket -} 
connect :: WebSocketRequest -> IO WebSocket
connect req = do
  mcb <- maybeCallback MessageEvent (onMessage req)
  ccb <- maybeCallback CloseEvent   (onClose req)
  withoutPreemption $ do
    ws <- case protocols req of
           []  -> js_createDefault (url req)
           [x] -> js_createStr     (url req) x
    (js_open ws mcb ccb >>= handleOpenErr >> return ws) `onException` js_close 1000 "Haskell Exception" ws

maybeCallback :: (JSVal -> a) -> Maybe (a -> IO ()) -> IO JSVal
maybeCallback _ Nothing = return jsNull
maybeCallback f (Just g) = do
  cb <- CB.syncCallback1 CB.ContinueAsync (g . f)
  return (jsval cb)

handleOpenErr :: JSVal -> IO ()
handleOpenErr r
  | isNull r  = return ()
  | otherwise = throwIO (userError "WebSocket failed to connect") -- fixme

{- | close a websocket and release the callbacks -}
close :: Maybe Int -> Maybe JSString -> WebSocket -> IO ()
close value reason ws =
  js_close (fromMaybe 1000 value) (fromMaybe JSS.empty reason) ws
{-# INLINE close #-}

send :: JSString -> WebSocket -> IO ()
send xs ws = js_send xs ws
{-# INLINE send #-}

sendBlob :: Blob -> WebSocket -> IO ()
sendBlob = js_sendBlob
{-# INLINE sendBlob #-}

sendArrayBuffer :: ArrayBuffer -> WebSocket -> IO ()
sendArrayBuffer = js_sendArrayBuffer
{-# INLINE sendArrayBuffer #-}

getBufferedAmount :: WebSocket -> IO Int
getBufferedAmount ws = js_getBufferedAmount ws
{-# INLINE getBufferedAmount #-}

getExtensions :: WebSocket -> IO JSString
getExtensions ws = js_getExtensions ws
{-# INLINE getExtensions #-}

getProtocol :: WebSocket -> IO JSString
getProtocol ws = js_getProtocol ws
{-# INLINE getProtocol #-}

getReadyState :: WebSocket -> IO ReadyState
getReadyState ws = fmap toEnum (js_getReadyState ws)
{-# INLINE getReadyState #-}

getBinaryType :: WebSocket -> IO BinaryType
getBinaryType ws = fmap toEnum (js_getBinaryType ws)
{-# INLINE getBinaryType #-}

getUrl :: WebSocket -> JSString
getUrl ws = js_getUrl ws
{-# INLINE getUrl #-}

getLastError :: WebSocket -> IO (Maybe ErrorEvent)
getLastError ws = do
  le <- js_getLastError ws
  return $ if isNull le then Nothing else Just (ErrorEvent le)
{-# INLINE getLastError #-}

setBinaryType :: BinaryType -> WebSocket -> IO ()
setBinaryType Blob = js_setBinaryType (JSS.pack "blob")
setBinaryType ArrayBuffer = js_setBinaryType (JSS.pack "arraybuffer")

-- -----------------------------------------------------------------------------

#if defined(wasm32_HOST_ARCH)
foreign import javascript unsafe "(($1) => { return new WebSocket($1); })($1)" js_createDefault :: JSString -> IO WebSocket
#else
foreign import javascript safe
  "(($1) => { return new WebSocket($1); })" js_createDefault :: JSString -> IO WebSocket
#endif
#if defined(wasm32_HOST_ARCH)
foreign import javascript unsafe "(($1,$2) => { return new WebSocket($1, $2); })($1,$2)" js_createStr :: JSString -> JSString -> IO WebSocket
#else
foreign import javascript safe
  "(($1,$2) => { return new WebSocket($1, $2); })" js_createStr :: JSString -> JSString -> IO WebSocket
#endif
#if defined(wasm32_HOST_ARCH)
foreign import javascript unsafe "(($1,$2) => { return new WebSocket($1, $2); })($1,$2)" js_createArr :: JSString -> JSArray -> IO WebSocket
#else
foreign import javascript safe
  "(($1,$2) => { return new WebSocket($1, $2); })" js_createArr :: JSString -> JSArray -> IO WebSocket
#endif

#if defined(wasm32_HOST_ARCH)
-- h$openWebSocket as an asynchronous import: resolves with null once the
-- socket is open, or with the error event if connecting fails.  (The
-- handlers are installed synchronously, before the socket can connect.)
-- Releasing the callbacks is left to the wasm runtime, see
-- GHC.JS.Foreign.Callback.releaseCallback.
foreign import javascript safe
  "var ws = $1; var mcb = $2; var ccb = $3; if(ws.readyState !== 0) { throw new Error('h$openWebSocket: unexpected readyState, socket must be CONNECTING'); } return new Promise((c) => { ws.lastError = null; ws.onopen = () => { if(mcb) { ws.onmessage = mcb; } if(ccb || mcb) { ws.onclose = (ce) => { if(ws.onmessage) { ws.onmessage = null; } if(ccb) { ccb(ce); } }; } ws.onerror = (err) => { ws.lastError = err; if(ws.onmessage) { ws.onmessage = null; } ws.close(); }; c(null); }; ws.onerror = (err) => { ws.onmessage = null; ws.close(); c(err); }; });"
  js_open_wasm :: WebSocket -> JSVal -> JSVal -> IO JSVal

-- wait until the socket has connected (or failed), like the JavaScript
-- backend's interruptible import
js_open  :: WebSocket -> JSVal -> JSVal -> IO JSVal
js_open ws mcb ccb = js_open_wasm ws mcb ccb >>= evaluate

-- h$closeWebSocket
foreign import javascript unsafe
  "var ws = $3; ws.onerror = null; if(ws.onmessage) { ws.onmessage = null; } ws.close($1, $2);"
  js_close :: Int -> JSString -> WebSocket -> IO ()
#else
foreign import javascript interruptible
  "h$openWebSocket"
  js_open  :: WebSocket -> JSVal -> JSVal -> IO JSVal
foreign import javascript safe
  "h$closeWebSocket"
  js_close :: Int -> JSString -> WebSocket -> IO ()
#endif
#if defined(wasm32_HOST_ARCH)
foreign import javascript unsafe "((x,y) => { y.send(x); })($1,$2)"          js_send              :: JSString -> WebSocket -> IO ()
#else
foreign import javascript unsafe
  "((x,y) => { y.send(x); })"          js_send              :: JSString -> WebSocket -> IO ()
#endif
#if defined(wasm32_HOST_ARCH)
foreign import javascript unsafe "((x,y) => { y.send(x); })($1,$2)"          js_sendBlob          :: Blob -> WebSocket -> IO ()
#else
foreign import javascript unsafe
  "((x,y) => { y.send(x); })"          js_sendBlob          :: Blob -> WebSocket -> IO ()
#endif
#if defined(wasm32_HOST_ARCH)
foreign import javascript unsafe "((x,y) => { y.send(x); })($1,$2)"          js_sendArrayBuffer   :: ArrayBuffer -> WebSocket -> IO ()
#else
foreign import javascript unsafe
  "((x,y) => { y.send(x); })"          js_sendArrayBuffer   :: ArrayBuffer -> WebSocket -> IO ()
#endif
#if defined(wasm32_HOST_ARCH)
foreign import javascript unsafe "((x) => { return x.bufferedAmount; })($1)"     js_getBufferedAmount :: WebSocket -> IO Int
#else
foreign import javascript unsafe
  "((x) => { return x.bufferedAmount; })"     js_getBufferedAmount :: WebSocket -> IO Int
#endif
#if defined(wasm32_HOST_ARCH)
foreign import javascript unsafe "((x) => { return x.readyState; })($1)"         js_getReadyState     :: WebSocket -> IO Int
#else
foreign import javascript unsafe
  "((x) => { return x.readyState; })"         js_getReadyState     :: WebSocket -> IO Int
#endif
#if defined(wasm32_HOST_ARCH)
foreign import javascript unsafe "((x) => { return x.protocol; })($1)"           js_getProtocol       :: WebSocket -> IO JSString
#else
foreign import javascript unsafe
  "((x) => { return x.protocol; })"           js_getProtocol       :: WebSocket -> IO JSString
#endif
#if defined(wasm32_HOST_ARCH)
foreign import javascript unsafe "((x) => { return x.extensions; })($1)"         js_getExtensions     :: WebSocket -> IO JSString
#else
foreign import javascript unsafe
  "((x) => { return x.extensions; })"         js_getExtensions     :: WebSocket -> IO JSString
#endif
#if defined(wasm32_HOST_ARCH)
foreign import javascript unsafe "((x) => { return x.url; })($1)"                js_getUrl            :: WebSocket -> JSString
#else
foreign import javascript unsafe
  "((x) => { return x.url; })"                js_getUrl            :: WebSocket -> JSString
#endif
#if defined(wasm32_HOST_ARCH)
foreign import javascript unsafe "((x) => { return x.binaryType === 'blob' ? 0 : 1; })($1)"
#else
foreign import javascript unsafe
  "((x) => { return x.binaryType === 'blob' ? 0 : 1; })"
#endif
  js_getBinaryType                             :: WebSocket -> IO Int
#if defined(wasm32_HOST_ARCH)
foreign import javascript unsafe "((x) => { return x.lastError; })($1)"          js_getLastError      :: WebSocket -> IO JSVal
#else
foreign import javascript unsafe
  "((x) => { return x.lastError; })"          js_getLastError      :: WebSocket -> IO JSVal
#endif

#if defined(wasm32_HOST_ARCH)
foreign import javascript unsafe "((x,y) => { y.binaryType = x; })($1,$2)"
#else
foreign import javascript unsafe
  "((x,y) => { y.binaryType = x; })"
#endif
  js_setBinaryType                             :: JSString -> WebSocket -> IO ()
