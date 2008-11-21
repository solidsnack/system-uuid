
{-| Utilities for fetching the results from foreign functions. 
 - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -}

{-# LANGUAGE ForeignFunctionInterface
  #-}

module System.UUID.FromForeign
  ( runAndRead
  ) where


import Data.UUID

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
  withForeignPtr fp $ peek . castPtr

