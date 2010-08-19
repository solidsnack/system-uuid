
{-| Version 4 'UUID' utilities. 
 -}

{-# LANGUAGE ForeignFunctionInterface
           , CPP
  #-}

module System.UUID.V4
  ( uuid
  ) where


import System.UUID.FromForeign

import Foreign.C
import Foreign.Ptr


{-| Obtain a Version 4 'UUID' with the native 'UUID' generator. 
 -}
uuid                         =  runAndRead native


#ifdef mingw32_HOST_OS

foreign import stdcall unsafe "UuidCreate"
  native                    ::  Ptr CChar -> IO ()

#else

foreign import ccall unsafe "uuid_generate_random"
  native                    ::  Ptr CChar -> IO ()

#endif

