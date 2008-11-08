
{-| Utilities for fetching the bytes from foreign computations.
 - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -}

{-# LANGUAGE ForeignFunctionInterface
  #-}

module System.UUID.Bytes where


import Foreign.C.String
import Foreign.C
import Foreign.ForeignPtr
import Foreign
import Data.Word


runAndRead                  ::  (Ptr CChar -> IO ()) -> IO [Word8]
runAndRead cProcedure        =  do
  fp                        <-  mallocForeignPtrArray 16
  withForeignPtr fp cProcedure
  withForeignPtr fp $ peekArray 16 . castPtr


