
{-| Version 1 'UUID' utilities. 
 -}

{-# LANGUAGE ForeignFunctionInterface
           , CPP
  #-}

module System.UUID.V1
  ( uuid
  ) where


import System.UUID.FromForeign

import Foreign.C
import Foreign.Ptr


{-| Obtain a Version 1 'UUID' with the native 'UUID' generator. 
 -}
uuid                         =  runAndRead native


#ifdef mingw32_HOST_OS

foreign import stdcall unsafe "UuidCreateSequential"
  native                    ::  Ptr CChar -> IO ()

#else

foreign import ccall unsafe "uuid_generate_time"
  native                    ::  Ptr CChar -> IO ()

#endif

