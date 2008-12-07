
{-| Macros of general use.
 - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -}

{-# LANGUAGE TemplateHaskell
  #-}


module Macros where


import MacroMacros

import Language.Haskell.TH
import Language.Haskell.TH.Syntax
import Data.List
import Control.Applicative
import Text.Regex


{-| Just a macro to pull in a file.
 -}
pullFile f                   =  lift =<< runIO (readFile f)


{-| Extract the version from the Cabal file and place it here as string.
 -}
version                      =  lift =<< do
  runIO $ do
    s                       <-  readFile "system-uuid.cabal"
    return $ case regex <//> s of
      Just [_, _, c]        ->  c
      _                     ->  ""
 where
  regex
    = ".*\nversion([\t ]*):([\t ]*)([[:digit:].]+)"


{-| Extract the usage from the module we're in and put it here.
 -}
usage                        =  lift =<< do
  p                         <-  $(presentFile)
  runIO $ do
    s                       <-  readFile p
    return $ extractUsage s 
 where
  fileName mod               =  map replace mod ++ ".hs"
  replace '.'                =  '/'
  replace c                  =  c


{-| Pulls the usage out of the comments in a file, digging through the file
    to find a comment with no text before the @SYNOPSIS@ or @USAGE@, and then
    treating all the text of the comment as the usage statement.
 -}
extractUsage s               =
  case regex <//> s of
    Just [_, b, _, d]       ->  ('\n':) . normalizeEmptyLines'' $ b ++ d
    _                       ->  ""
 where
  regex
    = ".*\\{-([\t -]*\n)+([ \t]+(SYNOPSIS|USAGE))(.+)\n[-\t ]*-\\}"


 -- normalizeLeadingEmptyLines
normalizeLeadingEmpties      =  ('\n':) . dropWhile (`elem` "\n \t")
normalizeEmptyLines''        =  reverse . normalizeLeadingEmpties . reverse


r <//> s                     =  matchRegex (mkRegexWithOpts r False True) s

