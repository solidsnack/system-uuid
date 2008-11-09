
{-| Obtain a Version 1 UUID from the system. 
 - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -}

{-# LANGUAGE ForeignFunctionInterface
           , CPP
  #-}

module System.UUID.V4
  ( uuid
  ) where


import System.UUID.FromForeign

import Foreign.C
import Foreign.Ptr


uuid                         =  runAndRead native


#ifdef mingw32_HOST_OS

foreign import stdcall unsafe "UuidCreate"
  native                    ::  Ptr CChar -> IO ()

#else

foreign import ccall unsafe "uuid_generate_random"
  native                    ::  Ptr CChar -> IO ()

#endif

