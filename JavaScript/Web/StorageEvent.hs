{-# LANGUAGE CPP #-}
{-# LANGUAGE ForeignFunctionInterface, JavaScriptFFI #-}

module JavaScript.Web.StorageEvent ( StorageEvent
                                   , key
                                   , oldValue
                                   , newValue
                                   , url
                                   , storageArea
                                   ) where

import Data.JSString
--import Data.JSString.Internal -- fixme
import Data.JSString.Internal.Type -- fixme

import GHCJS.Types

import JavaScript.Web.Storage.Internal

key :: StorageEvent -> Maybe JSString
key se | isNull r  = Nothing
       | otherwise = Just (JSString r)
  where
    r = js_getKey se
{-# INLINE key #-}

oldValue :: StorageEvent -> Maybe JSString
oldValue se | isNull r  = Nothing
            | otherwise = Just (JSString r)
  where
    r = js_getOldValue se
{-# INLINE oldValue #-}

newValue :: StorageEvent -> Maybe JSString
newValue se | isNull r  = Nothing
            | otherwise = Just (JSString r)
  where
    r = js_getNewValue se
{-# INLINE newValue #-}

url :: StorageEvent -> JSString
url se = js_getUrl se
{-# INLINE url #-}

storageArea :: StorageEvent -> Maybe Storage
storageArea se | isNull r  = Nothing
               | otherwise = Just (Storage r)
  where
    r = js_getStorageArea se
{-# INLINE storageArea #-}

-- -----------------------------------------------------------------------------

#if defined(wasm32_HOST_ARCH)
foreign import javascript unsafe "((x) => { return x.key; })($1)"         js_getKey         :: StorageEvent -> JSVal
#else
foreign import javascript unsafe
  "((x) => { return x.key; })"         js_getKey         :: StorageEvent -> JSVal
#endif
#if defined(wasm32_HOST_ARCH)
foreign import javascript unsafe "((x) => { return x.oldValue; })($1)"    js_getOldValue    :: StorageEvent -> JSVal
#else
foreign import javascript unsafe
  "((x) => { return x.oldValue; })"    js_getOldValue    :: StorageEvent -> JSVal
#endif
#if defined(wasm32_HOST_ARCH)
foreign import javascript unsafe "((x) => { return x.newValue; })($1)"    js_getNewValue    :: StorageEvent -> JSVal
#else
foreign import javascript unsafe
  "((x) => { return x.newValue; })"    js_getNewValue    :: StorageEvent -> JSVal
#endif
#if defined(wasm32_HOST_ARCH)
foreign import javascript unsafe "((x) => { return x.url; })($1)"         js_getUrl         :: StorageEvent -> JSString
#else
foreign import javascript unsafe
  "((x) => { return x.url; })"         js_getUrl         :: StorageEvent -> JSString
#endif
#if defined(wasm32_HOST_ARCH)
foreign import javascript unsafe "((x) => { return x.storageArea; })($1)" js_getStorageArea :: StorageEvent -> JSVal
#else
foreign import javascript unsafe
  "((x) => { return x.storageArea; })" js_getStorageArea :: StorageEvent -> JSVal
#endif
