
{-| Utilities for fetching the results from foreign functions. 
 - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -}

{-# LANGUAGE ForeignFunctionInterface
  #-}

module System.UUID.FromForeign
  ( runAndRead
  ) where


import Data.UUID.Bytes

import Foreign.C
import Foreign.ForeignPtr
import Foreign


{-| Allocate a pointer to capture the output of a foreign function, run the
 -  function and interpret the sixteen bytes following the pointer as a UUID.
 - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -}
runAndRead                  ::  (Ptr CChar -> IO ()) -> IO UUID 
runAndRead procedure         =  do
  fp                        <-  mallocForeignPtrArray 16
  withForeignPtr fp procedure 
  bytes                     <-  withForeignPtr fp $ peekArray 16 . castPtr
  let
    [x0, x1, x2, x3, x4, x5, x6, x7, x8, x9, xA, xB, xC, xD, xE, xF]
      = bytes
  return $ UUID x0 x1 x2 x3 x4 x5 x6 x7 x8 x9 xA xB xC xD xE xF

