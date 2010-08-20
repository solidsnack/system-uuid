
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


{-| Just a macro to pull in a file.
 -}
pullFile f                   =  lift =<< runIO (readFile f)


{-| Extract the version from the Cabal file and place it here as string.
 -}
version                      =  lift =<< do
  runIO $ do
    s                       <-  readFile "system-uuid.cabal"
    case filter version (lines s) of
      v:_                   ->  return (break_version v)
      [ ]                   ->  error "Could not find version :("
 where
  version line               =  "version" == take 7 line
  break_version = snd . break (/= ' ') . drop 1 . snd . break (== ':')


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
  case (begins_with_USAGE |--| terminal_comment) (lines s) of
    lines                   ->  unlines lines
    [   ]                   ->  error "Could not find USAGE :("
 where
  a |--| b                   =  takeWhile (not . b) . dropWhile (not . a)
  begins_with_USAGE line     =  " USAGE: " == take 8 line
  terminal_comment line      =  "-}" == drop (length line - 2) line


