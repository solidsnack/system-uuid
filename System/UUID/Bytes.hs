
{-| Utilities for fetching the bytes from foreign computations.
 - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -}

{-# LANGUAGE ForeignFunctionInterface
  #-}

module System.UUID.Bytes
  ( Bytes(..)
  , runAndRead
  ) where


import Foreign.C.String
import Foreign.C
import Foreign.ForeignPtr
import Foreign
import Data.Word
import Text.Printf


data Bytes                   =  Bytes
  !Word8 !Word8 !Word8 !Word8 !Word8 !Word8 !Word8 !Word8
  !Word8 !Word8 !Word8 !Word8 !Word8 !Word8 !Word8 !Word8
instance Show Bytes where
  show (Bytes x0 x1 x2 x3 x4 x5 x6 x7 x8 x9 xA xB xC xD xE xF)
    = concatMap (printf "%02.2x")
      [x0, x1, x2, x3, x4, x5, x6, x7, x8, x9, xA, xB, xC, xD, xE, xF]
instance Eq Bytes
instance Ord Bytes

runAndRead                  ::  (Ptr CChar -> IO ()) -> IO Bytes 
runAndRead cProcedure        =  do
  fp                        <-  mallocForeignPtrArray 16
  withForeignPtr fp cProcedure 
  bytes                     <-  withForeignPtr fp $ peekArray 16 . castPtr
  let
    [x0, x1, x2, x3, x4, x5, x6, x7, x8, x9, xA, xB, xC, xD, xE, xF]
      = bytes
  return $ Bytes x0 x1 x2 x3 x4 x5 x6 x7 x8 x9 xA xB xC xD xE xF

