
{-| Obtain a Version 1 UUID from the system. 
 - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -}

{-# LANGUAGE ForeignFunctionInterface
           , CPP
  #-}

module System.UUID.V1
  ( uuid
  ) where


import System.UUID.Bytes

import Foreign.C.String
import Foreign.C
import Foreign.ForeignPtr
import Foreign


#ifdef mingw32_HOST_OS

uuid                         =  runAndRead c

foreign import ccall unsafe "UuidCreateSequential"
  c                         ::  Ptr CChar -> IO ()

#else

uuid                         =  runAndRead c

foreign import ccall unsafe "uuid_generate_time"
  c                         ::  Ptr CChar -> IO ()

#endif

