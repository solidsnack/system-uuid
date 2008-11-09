

{-| Very simple IO module -- just the components that I actually need for
 -  String IO, and nothing more. (Prevents namespace conflict with ByteString
 -  IO elsewhere in the program).
 - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -}

module Messages
  ( (<<)
  , stdin
  , stdout
  , stderr
  ) where


import System.IO


{-| An @iostream@ style operator.
 - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -}
(<<)                         =  hPutStrLn
infixr 4 <<

