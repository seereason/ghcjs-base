{-# LANGUAGE CPP #-}
{-# LANGUAGE ForeignFunctionInterface #-}
{-# LANGUAGE JavaScriptFFI #-}
{-# LANGUAGE DeriveDataTypeable #-}

module JavaScript.Web.Location ( Location
                               , getWindowLocation
                               , getHref
                               , setHref
                               , getProtocol
                               , setProtocol
                               , getHost
                               , setHost
                               , getHostname
                               , setHostname
                               , getPort
                               , setPort
                               , getPathname
                               , setPathname
                               , getSearch
                               , setSearch
                               , getHash
                               , setHash
                               , getUsername
                               , setUsername
                               , getPassword
                               , setPassword
                               , getOrigin
                               , assign
                               , reload
                               , replace
                               ) where

import           Data.Typeable

import           Data.JSString (JSString)
import qualified Data.JSString as JSS

import           GHCJS.Types
#if defined(wasm32_HOST_ARCH)
-- the wasm JSFFI only unwraps newtypes whose constructors are in scope
import           Data.JSString.Internal.Type (JSString(..))
#endif

newtype Location = Location JSVal deriving (Typeable)
instance IsJSVal Location

getWindowLocation :: IO Location
getWindowLocation = js_getWindowLocation
{-# INLINE getWindowLocation #-}

getHref :: Location -> IO JSString
getHref = js_getHref
{-# INLINE getHref #-}

setHref :: JSString -> Location -> IO ()
setHref = js_setHref
{-# INLINE setHref #-}

getProtocol :: Location -> IO JSString
getProtocol = js_getProtocol
{-# INLINE getProtocol #-}

setProtocol :: JSString -> Location -> IO ()
setProtocol = js_setProtocol
{-# INLINE setProtocol #-}

getHost :: Location -> IO JSString
getHost = js_getHost
{-# INLINE getHost #-}

setHost :: JSString -> Location -> IO ()
setHost = js_setHost
{-# INLINE setHost #-}

getHostname :: Location -> IO JSString
getHostname = js_getHostname
{-# INLINE getHostname #-}

setHostname :: JSString -> Location -> IO ()
setHostname = js_setHostname
{-# INLINE setHostname #-}

getPort :: Location -> IO JSString
getPort = js_getPort
{-# INLINE getPort #-}

setPort :: JSString -> Location -> IO ()
setPort = js_setPort
{-# INLINE setPort #-}

getPathname :: Location -> IO JSString
getPathname = js_getPathname
{-# INLINE getPathname #-}

setPathname :: JSString -> Location -> IO ()
setPathname = js_setPathname
{-# INLINE setPathname #-}

getSearch :: Location -> IO JSString
getSearch = js_getSearch
{-# INLINE getSearch #-}

setSearch :: JSString -> Location -> IO ()
setSearch = js_setSearch
{-# INLINE setSearch #-}

getHash :: Location -> IO JSString
getHash = js_getHash
{-# INLINE getHash #-}

setHash :: JSString -> Location -> IO ()
setHash = js_setHash
{-# INLINE setHash #-}

getUsername :: Location -> IO JSString
getUsername = js_getUsername
{-# INLINE getUsername #-}

setUsername :: JSString -> Location -> IO ()
setUsername = js_setUsername
{-# INLINE setUsername #-}

getPassword :: Location -> IO JSString
getPassword = js_getPassword
{-# INLINE getPassword #-}

setPassword :: JSString -> Location -> IO ()
setPassword = js_setPassword
{-# INLINE setPassword #-}

getOrigin :: Location -> IO JSString
getOrigin = js_getUsername
{-# INLINE getOrigin #-}

assign :: JSString -> Location -> IO ()
assign = js_assign
{-# INLINE assign #-}

reload :: Bool -> Location -> IO ()
reload = js_reload
{-# INLINE reload #-}

replace :: JSString -> Location -> IO ()
replace = js_assign
{-# INLINE replace #-}

-------------------------------------------------------------------------------

#if defined(wasm32_HOST_ARCH)
foreign import javascript unsafe "window.location" js_getWindowLocation :: IO Location
#else
foreign import javascript safe "window.location" js_getWindowLocation :: IO Location
#endif

#if defined(wasm32_HOST_ARCH)
foreign import javascript unsafe "((x) => { return x.href; })($1)"     js_getHref     :: Location -> IO JSString
#else
foreign import javascript unsafe "((x) => { return x.href; })"     js_getHref     :: Location -> IO JSString
#endif
#if defined(wasm32_HOST_ARCH)
foreign import javascript unsafe "((x) => { return x.protocol; })($1)" js_getProtocol :: Location -> IO JSString
#else
foreign import javascript unsafe "((x) => { return x.protocol; })" js_getProtocol :: Location -> IO JSString
#endif
#if defined(wasm32_HOST_ARCH)
foreign import javascript unsafe "((x) => { return x.host; })($1)"     js_getHost     :: Location -> IO JSString
#else
foreign import javascript unsafe "((x) => { return x.host; })"     js_getHost     :: Location -> IO JSString
#endif
#if defined(wasm32_HOST_ARCH)
foreign import javascript unsafe "((x) => { return x.hostname; })($1)" js_getHostname :: Location -> IO JSString
#else
foreign import javascript unsafe "((x) => { return x.hostname; })" js_getHostname :: Location -> IO JSString
#endif
#if defined(wasm32_HOST_ARCH)
foreign import javascript unsafe "((x) => { return x.port; })($1)"     js_getPort     :: Location -> IO JSString
#else
foreign import javascript unsafe "((x) => { return x.port; })"     js_getPort     :: Location -> IO JSString
#endif
#if defined(wasm32_HOST_ARCH)
foreign import javascript unsafe "((x) => { return x.pathname; })($1)" js_getPathname :: Location -> IO JSString
#else
foreign import javascript unsafe "((x) => { return x.pathname; })" js_getPathname :: Location -> IO JSString
#endif
#if defined(wasm32_HOST_ARCH)
foreign import javascript unsafe "((x) => { return x.search; })($1)"   js_getSearch   :: Location -> IO JSString
#else
foreign import javascript unsafe "((x) => { return x.search; })"   js_getSearch   :: Location -> IO JSString
#endif
#if defined(wasm32_HOST_ARCH)
foreign import javascript unsafe "((x) => { return x.hash; })($1)"     js_getHash     :: Location -> IO JSString
#else
foreign import javascript unsafe "((x) => { return x.hash; })"     js_getHash     :: Location -> IO JSString
#endif
#if defined(wasm32_HOST_ARCH)
foreign import javascript unsafe "((x) => { return x.username; })($1)" js_getUsername :: Location -> IO JSString
#else
foreign import javascript unsafe "((x) => { return x.username; })" js_getUsername :: Location -> IO JSString
#endif
#if defined(wasm32_HOST_ARCH)
foreign import javascript unsafe "((x) => { return x.password; })($1)" js_getPassword :: Location -> IO JSString
#else
foreign import javascript unsafe "((x) => { return x.password; })" js_getPassword :: Location -> IO JSString
#endif
#if defined(wasm32_HOST_ARCH)
foreign import javascript unsafe "((x) => { return x.origin; })($1)"   js_getOrigin   :: Location -> IO JSString
#else
foreign import javascript unsafe "((x) => { return x.origin; })"   js_getOrigin   :: Location -> IO JSString
#endif

#if defined(wasm32_HOST_ARCH)
foreign import javascript unsafe "((x,y) => { y.href = x; })($1,$2)"     js_setHref     :: JSString -> Location -> IO ()
#else
foreign import javascript safe "((x,y) => { y.href = x; })"     js_setHref     :: JSString -> Location -> IO ()
#endif
#if defined(wasm32_HOST_ARCH)
foreign import javascript unsafe "((x,y) => { y.protocol = x; })($1,$2)" js_setProtocol :: JSString -> Location -> IO ()
#else
foreign import javascript safe "((x,y) => { y.protocol = x; })" js_setProtocol :: JSString -> Location -> IO ()
#endif
#if defined(wasm32_HOST_ARCH)
foreign import javascript unsafe "((x,y) => { y.host = x; })($1,$2)"     js_setHost     :: JSString -> Location -> IO ()
#else
foreign import javascript safe "((x,y) => { y.host = x; })"     js_setHost     :: JSString -> Location -> IO ()
#endif
#if defined(wasm32_HOST_ARCH)
foreign import javascript unsafe "((x,y) => { y.hostname = x; })($1,$2)" js_setHostname :: JSString -> Location -> IO ()
#else
foreign import javascript safe "((x,y) => { y.hostname = x; })" js_setHostname :: JSString -> Location -> IO ()
#endif
#if defined(wasm32_HOST_ARCH)
foreign import javascript unsafe "((x,y) => { y.port = x; })($1,$2)"     js_setPort     :: JSString -> Location -> IO ()
#else
foreign import javascript safe "((x,y) => { y.port = x; })"     js_setPort     :: JSString -> Location -> IO ()
#endif
#if defined(wasm32_HOST_ARCH)
foreign import javascript unsafe "((x,y) => { y.pathname = x; })($1,$2)" js_setPathname :: JSString -> Location -> IO ()
#else
foreign import javascript safe "((x,y) => { y.pathname = x; })" js_setPathname :: JSString -> Location -> IO ()
#endif
#if defined(wasm32_HOST_ARCH)
foreign import javascript unsafe "((x,y) => { y.search = x; })($1,$2)"   js_setSearch   :: JSString -> Location -> IO ()
#else
foreign import javascript safe "((x,y) => { y.search = x; })"   js_setSearch   :: JSString -> Location -> IO ()
#endif
#if defined(wasm32_HOST_ARCH)
foreign import javascript unsafe "((x,y) => { y.hash = x; })($1,$2)"     js_setHash     :: JSString -> Location -> IO ()
#else
foreign import javascript safe "((x,y) => { y.hash = x; })"     js_setHash     :: JSString -> Location -> IO ()
#endif
#if defined(wasm32_HOST_ARCH)
foreign import javascript unsafe "((x,y) => { y.username = x; })($1,$2)" js_setUsername :: JSString -> Location -> IO ()
#else
foreign import javascript safe "((x,y) => { y.username = x; })" js_setUsername :: JSString -> Location -> IO ()
#endif
#if defined(wasm32_HOST_ARCH)
foreign import javascript unsafe "((x,y) => { y.password = x; })($1,$2)" js_setPassword :: JSString -> Location -> IO ()
#else
foreign import javascript safe "((x,y) => { y.password = x; })" js_setPassword :: JSString -> Location -> IO ()
#endif

#if defined(wasm32_HOST_ARCH)
foreign import javascript unsafe "((x,y) => { y.assign(x); })($1,$2)"    js_assign      :: JSString -> Location -> IO ()
#else
foreign import javascript safe "((x,y) => { y.assign(x); })"    js_assign      :: JSString -> Location -> IO ()
#endif
#if defined(wasm32_HOST_ARCH)
foreign import javascript unsafe "((x,y) => { y.reload(x); })($1,$2)"    js_reload      :: Bool     -> Location -> IO ()
#else
foreign import javascript safe "((x,y) => { y.reload(x); })"    js_reload      :: Bool     -> Location -> IO ()
#endif
#if defined(wasm32_HOST_ARCH)
foreign import javascript unsafe "((x,y) => { y.replace(x); })($1,$2)"   js_replace     :: JSString -> Location -> IO ()
#else
foreign import javascript safe "((x,y) => { y.replace(x); })"   js_replace     :: JSString -> Location -> IO ()
#endif
