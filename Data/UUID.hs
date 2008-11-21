
{-| A 'UUID' and its many instances. 
 - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -}

module Data.UUID
  ( UUID()
  ) where


import Data.Word
import Data.Char
import Data.Binary
import Data.Binary.Put
import Data.Binary.Get
import Data.Bits
import Foreign.C
import Foreign.ForeignPtr
import Foreign
import Control.Monad
import Control.Applicative
import Numeric
import Text.ParserCombinators.ReadPrec (lift)
import Text.ParserCombinators.ReadP
import Text.Read hiding (pfail)
import Data.List
import Text.Printf


data UUID                    =  UUID
  !Word8 !Word8 !Word8 !Word8 !Word8 !Word8 !Word8 !Word8
  !Word8 !Word8 !Word8 !Word8 !Word8 !Word8 !Word8 !Word8
instance Eq UUID
instance Ord UUID
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
instance Storable UUID where
  sizeOf _                   =  16
  alignment _                =  4
  peek p                     =  do
    bytes                   <-  peekArray 16 $ castPtr p
    return $ fromList bytes
  poke p uuid                =  pokeArray (castPtr p) $ listOfBytes uuid 
instance Num UUID where
  fromInteger i             --  This really should be in a different class.
    | i <= 0                 =  UUID  0 0 0 0  0 0 0 0  0 0 0 0  0 0 0 0
    | i > 2^128              =  UUID  0 0 0 0  0 0 0 0  0 0 0 0  0 0 0 0
    | otherwise              =  fromList bytes 
   where
    bytes                    =  map shifter $ reverse [0,8..15*8]
     where
      shifter n              =  fromInteger $ i `shiftR` (8 * n)
  (+)                        =  undefined
  (-)                        =  undefined
  (*)                        =  undefined
  negate                     =  undefined
  abs                        =  undefined
  signum                     =  undefined
instance Bounded UUID where
  minBound                   =  0
  maxBound                   =  2^128
instance Binary UUID where
  put                        =  mapM_ putWord8 . listOfBytes 
  get                        =  fromList <$> sequence (replicate 16 getWord8) 


listOfBytes (UUID x0 x1 x2 x3 x4 x5 x6 x7 x8 x9 xA xB xC xD xE xF)
  = [ x0, x1, x2, x3, x4, x5, x6, x7, x8, x9, xA, xB, xC, xD, xE, xF ]

fromList [ x0, x1, x2, x3, x4, x5, x6, x7, x8, x9, xA, xB, xC, xD, xE, xF ]
  = (UUID x0 x1 x2 x3 x4 x5 x6 x7 x8 x9 xA xB xC xD xE xF)

