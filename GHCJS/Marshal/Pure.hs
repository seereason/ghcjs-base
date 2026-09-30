{-# LANGUAGE CPP #-}
{-# LANGUAGE DefaultSignatures #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE DefaultSignatures #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TupleSections #-}
{-# LANGUAGE MagicHash #-}
{-# LANGUAGE JavaScriptFFI #-}
{-# LANGUAGE ForeignFunctionInterface #-}
{-# LANGUAGE UnliftedFFITypes #-}
{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveDataTypeable #-}

{-
  experimental pure marshalling for lighter weight interaction in the quasiquoter
 -}
module GHCJS.Marshal.Pure ( PFromJSVal(..)
                          , PToJSVal(..)
                          , jsvalToChar
                          , charToJSVal
                          ) where

import           Data.Char (chr, ord)
import           Data.Data
import           Data.Int (Int8, Int16, Int32)
import           Data.JSString.Internal.Type
import           Data.Maybe
import           Data.Text (Text)
import           Data.Typeable
import           Data.Word (Word8, Word16, Word32, Word)
import           Data.JSString
import           Data.JSString.Text
import           Data.Bits ((.&.))
import           Unsafe.Coerce (unsafeCoerce)
import           GHC.Int
import           GHC.Word
import           GHC.Types
import           GHC.Float
import           GHC.Prim

import           GHCJS.Types
import qualified GHC.JS.Prim as Prim
import           GHCJS.Foreign.Internal
import           GHCJS.Marshal.Internal

{-
type family IsPureShared a where
  IsPureShared PureExclusive = False
  IsPureShared PureShared    = True

type family IsPureExclusive a where
  IsPureExclusive PureExclusive = True
  IsPureExclusive PureShared    = True
  -}

instance PFromJSVal JSVal where pFromJSVal = id
                                {-# INLINE pFromJSVal #-}
instance PFromJSVal ()    where pFromJSVal _ = ()
                                {-# INLINE pFromJSVal #-}

instance PFromJSVal JSString where pFromJSVal = JSString
                                   {-# INLINE pFromJSVal #-}
instance PFromJSVal [Char] where pFromJSVal   = Prim.fromJSString
                                 {-# INLINE pFromJSVal #-}
instance PFromJSVal Text   where pFromJSVal   = textFromJSVal
                                 {-# INLINE pFromJSVal #-}
instance PFromJSVal Char   where pFromJSVal x = C# (jsvalToChar x)
                                 {-# INLINE pFromJSVal #-}
instance PFromJSVal Bool   where pFromJSVal   = isTruthy
                                 {-# INLINE pFromJSVal #-}
instance PFromJSVal Int    where pFromJSVal x = I# (jsvalToInt x)
                                 {-# INLINE pFromJSVal #-}
instance PFromJSVal Int8   where pFromJSVal x = I8# (jsvalToInt8 x)
                                 {-# INLINE pFromJSVal #-}
instance PFromJSVal Int16  where pFromJSVal x = I16# (jsvalToInt16 x)
                                 {-# INLINE pFromJSVal #-}
instance PFromJSVal Int32  where pFromJSVal x = I32# (jsvalToInt32 x)
                                 {-# INLINE pFromJSVal #-}
instance PFromJSVal Word   where pFromJSVal x = W# (jsvalToWord x)
                                 {-# INLINE pFromJSVal #-}
instance PFromJSVal Word8  where pFromJSVal x = W8# (jsvalToWord8 x)
                                 {-# INLINE pFromJSVal #-}
instance PFromJSVal Word16 where pFromJSVal x = W16# (jsvalToWord16 x)
                                 {-# INLINE pFromJSVal #-}
instance PFromJSVal Word32 where pFromJSVal x = (jsvalToWord32 x)
                                 {-# INLINE pFromJSVal #-}
instance PFromJSVal Float  where pFromJSVal x = F# (jsvalToFloat x)
                                 {-# INLINE pFromJSVal #-}
instance PFromJSVal Double where pFromJSVal x = D# (jsvalToDouble x)
                                 {-# INLINE pFromJSVal #-}

instance PFromJSVal a => PFromJSVal (Maybe a) where
    pFromJSVal x | isUndefined x || isNull x = Nothing
    pFromJSVal x = Just (pFromJSVal x)
    {-# INLINE pFromJSVal #-}

instance PToJSVal JSVal     where pToJSVal = id
                                  {-# INLINE pToJSVal #-}
instance PToJSVal JSString  where pToJSVal          = jsval
                                  {-# INLINE pToJSVal #-}
instance PToJSVal [Char]    where pToJSVal          = Prim.toJSString
                                  {-# INLINE pToJSVal #-}
instance PToJSVal Text      where pToJSVal          = jsval . textToJSString
                                  {-# INLINE pToJSVal #-}
instance PToJSVal Char      where pToJSVal (C# c)   = charToJSVal c
                                  {-# INLINE pToJSVal #-}
instance PToJSVal Bool      where pToJSVal True     = jsTrue
                                  pToJSVal False    = jsFalse
                                  {-# INLINE pToJSVal #-}
instance PToJSVal Int       where pToJSVal (I# x)   = intToJSVal x
                                  {-# INLINE pToJSVal #-}
instance PToJSVal Int8      where pToJSVal (I8# x)  = intToJSVal (int8ToInt# x)
                                  {-# INLINE pToJSVal #-}
instance PToJSVal Int16     where pToJSVal (I16# x) = intToJSVal (int16ToInt# x)
                                  {-# INLINE pToJSVal #-}
instance PToJSVal Int32     where pToJSVal (I32# x) = intToJSVal (int32ToInt# x)
                                  {-# INLINE pToJSVal #-}
instance PToJSVal Word      where pToJSVal (W# x)   = wordToJSVal x
                                  {-# INLINE pToJSVal #-}
instance PToJSVal Word8     where pToJSVal (W8# x)  = wordToJSVal (word8ToWord# x)
                                  {-# INLINE pToJSVal #-}
instance PToJSVal Word16    where pToJSVal (W16# x) = wordToJSVal (word16ToWord# x)
                                  {-# INLINE pToJSVal #-}
instance PToJSVal Word32    where pToJSVal (W32# x) = wordToJSVal (word32ToWord# x)
                                  {-# INLINE pToJSVal #-}
instance PToJSVal Float     where pToJSVal (F# x)   = floatToJSVal x
                                  {-# INLINE pToJSVal #-}
instance PToJSVal Double    where pToJSVal (D# x)   = doubleToJSVal x
                                  {-# INLINE pToJSVal #-}

instance PToJSVal a => PToJSVal (Maybe a) where
    pToJSVal Nothing  = jsNull
    pToJSVal (Just a) = pToJSVal a
    {-# INLINE pToJSVal #-}

#if defined(wasm32_HOST_ARCH)
foreign import javascript unsafe "((x) => { return x>>>0; })($1)"
  jsvalToWord_wasm :: JSVal -> Word
jsvalToWord :: JSVal -> Word#
jsvalToWord a1 = case jsvalToWord_wasm a1 of W# r -> r
{-# INLINE jsvalToWord #-}
#else
foreign import javascript unsafe "((x) => { return x>>>0; })"        jsvalToWord   :: JSVal -> Word#
#endif
#if defined(wasm32_HOST_ARCH)
foreign import javascript unsafe "$1&0xff"   jsvalToWord8_wasm  :: JSVal -> Word8
foreign import javascript unsafe "$1&0xffff" jsvalToWord16_wasm :: JSVal -> Word16
jsvalToWord8 :: JSVal -> Word8#
jsvalToWord8 a1 = case jsvalToWord8_wasm a1 of W8# r -> r
{-# INLINE jsvalToWord8 #-}
jsvalToWord16 :: JSVal -> Word16#
jsvalToWord16 a1 = case jsvalToWord16_wasm a1 of W16# r -> r
{-# INLINE jsvalToWord16 #-}
#else
foreign import javascript unsafe "((x) => { return x&0xff; })"       jsvalToWord8  :: JSVal -> Word8#
foreign import javascript unsafe "((x) => { return x&0xffff; })"     jsvalToWord16 :: JSVal -> Word16#
#endif
#if defined(wasm32_HOST_ARCH)
foreign import javascript unsafe "((x) => { return x>>>0; })($1)"        jsvalToWord32 :: JSVal -> Word32
#else
foreign import javascript unsafe "((x) => { return x>>>0; })"        jsvalToWord32 :: JSVal -> Word32
#endif
#if defined(wasm32_HOST_ARCH)
foreign import javascript unsafe "((x) => { return x|0; })($1)"
  jsvalToInt_wasm :: JSVal -> Int
jsvalToInt :: JSVal -> Int#
jsvalToInt a1 = case jsvalToInt_wasm a1 of I# r -> r
{-# INLINE jsvalToInt #-}
#else
foreign import javascript unsafe "((x) => { return x|0; })"          jsvalToInt    :: JSVal -> Int#
#endif
#if defined(wasm32_HOST_ARCH)
foreign import javascript unsafe "$1<<24>>24" jsvalToInt8_wasm  :: JSVal -> Int8
foreign import javascript unsafe "$1<<16>>16" jsvalToInt16_wasm :: JSVal -> Int16
foreign import javascript unsafe "$1|0"       jsvalToInt32_wasm :: JSVal -> Int32
jsvalToInt8 :: JSVal -> Int8#
jsvalToInt8 a1 = case jsvalToInt8_wasm a1 of I8# r -> r
{-# INLINE jsvalToInt8 #-}
jsvalToInt16 :: JSVal -> Int16#
jsvalToInt16 a1 = case jsvalToInt16_wasm a1 of I16# r -> r
{-# INLINE jsvalToInt16 #-}
jsvalToInt32 :: JSVal -> Int32#
jsvalToInt32 a1 = case jsvalToInt32_wasm a1 of I32# r -> r
{-# INLINE jsvalToInt32 #-}
#else
foreign import javascript unsafe "((x) => { return x<<24>>24; })"    jsvalToInt8   :: JSVal -> Int8#
foreign import javascript unsafe "((x) => { return x<<16>>16; })"    jsvalToInt16  :: JSVal -> Int16#
foreign import javascript unsafe "((x) => { return x|0; })"          jsvalToInt32  :: JSVal -> Int32#
#endif
#if defined(wasm32_HOST_ARCH)
foreign import javascript unsafe "((x) => { return +x; })($1)"
  jsvalToFloat_wasm :: JSVal -> Float
jsvalToFloat :: JSVal -> Float#
jsvalToFloat a1 = case jsvalToFloat_wasm a1 of F# r -> r
{-# INLINE jsvalToFloat #-}
#else
foreign import javascript unsafe "((x) => { return +x; })"           jsvalToFloat  :: JSVal -> Float#
#endif
#if defined(wasm32_HOST_ARCH)
foreign import javascript unsafe "((x) => { return +x; })($1)"
  jsvalToDouble_wasm :: JSVal -> Double
jsvalToDouble :: JSVal -> Double#
jsvalToDouble a1 = case jsvalToDouble_wasm a1 of D# r -> r
{-# INLINE jsvalToDouble #-}
#else
foreign import javascript unsafe "((x) => { return +x; })"           jsvalToDouble :: JSVal -> Double#
#endif
#if defined(wasm32_HOST_ARCH)
foreign import javascript unsafe "((x) => { return x&0x7fffffff; })($1)"
  jsvalToChar_wasm :: JSVal -> Char
jsvalToChar :: JSVal -> Char#
jsvalToChar a1 = case jsvalToChar_wasm a1 of C# r -> r
{-# INLINE jsvalToChar #-}
#else
foreign import javascript unsafe "((x) => { return x&0x7fffffff; })" jsvalToChar   :: JSVal -> Char#
#endif

#if defined(wasm32_HOST_ARCH)
foreign import javascript unsafe "((x) => { return x; })($1)"
  wordToJSVal_wasm :: Word -> JSVal
wordToJSVal :: Word#   -> JSVal
wordToJSVal a1 = wordToJSVal_wasm (W# a1)
{-# INLINE wordToJSVal #-}
#else
foreign import javascript unsafe "((x) => { return x; })" wordToJSVal   :: Word#   -> JSVal
#endif
#if defined(wasm32_HOST_ARCH)
foreign import javascript unsafe "((x) => { return x; })($1)"
  intToJSVal_wasm :: Int -> JSVal
intToJSVal :: Int#    -> JSVal
intToJSVal a1 = intToJSVal_wasm (I# a1)
{-# INLINE intToJSVal #-}
#else
foreign import javascript unsafe "((x) => { return x; })" intToJSVal    :: Int#    -> JSVal
#endif
#if defined(wasm32_HOST_ARCH)
foreign import javascript unsafe "((x) => { return x; })($1)"
  doubleToJSVal_wasm :: Double -> JSVal
doubleToJSVal :: Double# -> JSVal
doubleToJSVal a1 = doubleToJSVal_wasm (D# a1)
{-# INLINE doubleToJSVal #-}
#else
foreign import javascript unsafe "((x) => { return x; })" doubleToJSVal :: Double# -> JSVal
#endif
#if defined(wasm32_HOST_ARCH)
foreign import javascript unsafe "((x) => { return x; })($1)"
  floatToJSVal_wasm :: Float -> JSVal
floatToJSVal :: Float#  -> JSVal
floatToJSVal a1 = floatToJSVal_wasm (F# a1)
{-# INLINE floatToJSVal #-}
#else
foreign import javascript unsafe "((x) => { return x; })" floatToJSVal  :: Float#  -> JSVal
#endif
#if defined(wasm32_HOST_ARCH)
foreign import javascript unsafe "((x) => { return x; })($1)"
  charToJSVal_wasm :: Char -> JSVal
charToJSVal :: Char#   -> JSVal
charToJSVal a1 = charToJSVal_wasm (C# a1)
{-# INLINE charToJSVal #-}
#else
foreign import javascript unsafe "((x) => { return x; })" charToJSVal   :: Char#   -> JSVal
#endif

