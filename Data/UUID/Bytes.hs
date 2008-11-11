
{-| A 'UUID' as plain old bytes.
 - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -}

module Data.UUID.Bytes
  ( UUID(..)
  , listOfBytes
  ) where


import Data.Word
import Data.Char
import Control.Monad
import Numeric
import Text.ParserCombinators.ReadPrec (lift)
import Text.ParserCombinators.ReadP
import Text.Read hiding (pfail)
import Data.List
import Text.Printf


data UUID                    =  UUID
  !Word8 !Word8 !Word8 !Word8 !Word8 !Word8 !Word8 !Word8
  !Word8 !Word8 !Word8 !Word8 !Word8 !Word8 !Word8 !Word8
instance Show UUID where
  show (UUID x0 x1 x2 x3 x4 x5 x6 x7 x8 x9 xA xB xC xD xE xF)
    = printf formatUUID x0 x1 x2 x3 x4 x5 x6 x7 x8 x9 xA xB xC xD xE xF
   where
    formatUUID               =  intercalate "-" $ map b [ 2, 1, 1, 1, 3 ]
    b                        =  concat . (`replicate` "%02.2x%02.2x")
instance Read UUID where
  readPrec                   =  lift $ do
    [x0,x1,x2,x3]           <-  count 4 byte
    char '-'
    [x4,x5]                 <-  count 2 byte
    char '-'
    [x6,x7]                 <-  count 2 byte
    char '-'
    [x8,x9]                 <-  count 2 byte
    char '-'
    [xA,xB,xC,xD,xE,xF]     <-  count 6 byte
    return $ UUID x0 x1 x2 x3 x4 x5 x6 x7 x8 x9 xA xB xC xD xE xF
   where
    byte                     =  do
      s                     <-  sequence $ replicate 2 $ satisfy isHexDigit
      case readHex s of
        [(b, _)]            ->  return b
        _                   ->  pfail
instance Eq UUID
instance Ord UUID


listOfBytes (UUID x0 x1 x2 x3 x4 x5 x6 x7 x8 x9 xA xB xC xD xE xF)
  = [ x0, x1, x2, x3, x4, x5, x6, x7, x8, x9, xA, xB, xC, xD, xE, xF ]

