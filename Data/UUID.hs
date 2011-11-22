{-# LANGUAGE DeriveDataTypeable
  #-}
{-| The 'UUID' datatype.
 -}

module Data.UUID
  ( UUID()
  , asWord64s
  , asWord32s
  ) where


import Data.Word
import Data.Char
import Data.Binary
import Data.Binary.Put
import Data.Binary.Get
import Data.Bits
import qualified Data.ByteString.Lazy
import Data.Digest.Murmur32
import Data.Digest.Murmur64
import Data.String
import Data.Typeable
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


{-| A type for Uniform Unique Identifiers. The 'Num' instance allows 'UUID's
    to be specified with @0@, @1@, &c. -- testing for the null 'UUID' is
    easier that way. The 'Storable' instance is compatible with most (all?)
    systems' native representation of 'UUID's.
 -}
data UUID                    =  UUID
  !Word8 !Word8 !Word8 !Word8 !Word8 !Word8 !Word8 !Word8
  !Word8 !Word8 !Word8 !Word8 !Word8 !Word8 !Word8 !Word8
 deriving (Eq, Ord, Typeable)
instance Show UUID where
  show (UUID x0 x1 x2 x3 x4 x5 x6 x7 x8 x9 xA xB xC xD xE xF)
    = printf formatUUID x0 x1 x2 x3 x4 x5 x6 x7 x8 x9 xA xB xC xD xE xF
   where
    formatUUID               =  intercalate "-" $ map b [ 2, 1, 1, 1, 3 ]
    b                        =  concat . (`replicate` "%02.2x%02.2x")
instance Read UUID where
  readPrec                   =  lift $ do
    skipSpaces
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
instance IsString UUID where
  fromString                 =  read
instance Storable UUID where
  sizeOf _                   =  16
  alignment _                =  4
  peek p                     =  do
    bytes                   <-  peekArray 16 $ castPtr p
    return $ fromList bytes
  poke p uuid                =  pokeArray (castPtr p) $ listOfBytes uuid
instance Bounded UUID where
  minBound                   =  UUID 0x00 0x00 0x00 0x00 0x00 0x00 0x00 0x00
                                     0x00 0x00 0x00 0x00 0x00 0x00 0x00 0x00
  maxBound                   =  UUID 0xff 0xff 0xff 0xff 0xff 0xff 0xff 0xff
                                     0xff 0xff 0xff 0xff 0xff 0xff 0xff 0xff
instance Binary UUID where
  put                        =  mapM_ putWord8 . listOfBytes
  get                        =  fromList <$> sequence (replicate 16 getWord8)
instance Hashable32 UUID where
  hash32Add                  =  hash32Add . asWord32s
instance Hashable64 UUID where
  hash64Add                  =  hash64Add . asWord64s


listOfBytes (UUID x0 x1 x2 x3 x4 x5 x6 x7 x8 x9 xA xB xC xD xE xF)
  = [ x0, x1, x2, x3, x4, x5, x6, x7, x8, x9, xA, xB, xC, xD, xE, xF ]

fromList [ x0, x1, x2, x3, x4, x5, x6, x7, x8, x9, xA, xB, xC, xD, xE, xF ]
  = UUID x0 x1 x2 x3 x4 x5 x6 x7 x8 x9 xA xB xC xD xE xF
fromList _                   =  minBound

asWord64s                   ::  UUID -> (Word64, Word64)
asWord64s uuid               =  (decode front, decode back)
 where
  (front, back)              =  Data.ByteString.Lazy.splitAt 8 $ encode uuid

asWord32s                   ::  UUID -> (Word32, Word32, Word32, Word32)
asWord32s uuid = (decode front', decode front'', decode back', decode back'')
 where
  (front, back)              =  Data.ByteString.Lazy.splitAt 8 $ encode uuid
  (front', front'')          =  Data.ByteString.Lazy.splitAt 4 front
  (back', back'')            =  Data.ByteString.Lazy.splitAt 4 back

