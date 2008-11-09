
{-| Macros of general use.
 - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -}

{-# LANGUAGE TemplateHaskell
  #-}


module Macros where


import Language.Haskell.TH
import Language.Haskell.TH.Syntax
import Data.List
import Text.Regex


{-| Just a macro to pull in a file.
 - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -}
pullFile f                   =  lift =<< runIO (readFile f)


{-| Extract the usage from the module we're in and put it here.
 - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -}
usage                        =  lift =<< do
  mod                       <-  currentModule
  runIO $ do
    s                       <-  readFile $ fileName mod
    return $ extractUsage s 
 where
  fileName mod               =  map replace mod ++ ".hs"
  replace '.'                =  '/'
  replace c                  =  c


{-| Pulls the usage out of the comments in a file, digging through the file
 -  to find a comment with no text before the @SYNOPSIS@ or @USAGE@, and then
 -  treating all the text of the comment as the usage statement.
 - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -}
extractUsage s               =
  case regex <//> s of
    Just [_, a, _, b]       ->  ('\n':) . normalizeEmptyLines'' $ a ++ b
    _                       ->  ""
 where
  r <//> s                   =  matchRegex (mkRegexWithOpts r False True) s
  regex = ".*\\{-([\t -]*\n)+([ \t]+(SYNOPSIS|USAGE))(.+)\n[-\t ]*-\\}"

 -- normalizeLeadingEmptyLines
normalizeLeadingEmpties      =  ('\n':) . dropWhile (`elem` "\n \t")
normalizeEmptyLines''        =  reverse . normalizeLeadingEmpties . reverse




