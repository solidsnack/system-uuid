
module Options
  ( option
  , anyString
  , string
  , initialChar
  , module Text.ParserCombinators.Parsec
  ) where


import Text.ParserCombinators.Parsec hiding
  ( parse
  , string
  , oneOf
  , option
  , (<|>)
  )
import Control.Monad
import Control.Monad.Instances


noMore                       =  string "--"


stringPrim                   =  tokenPrim show nextPos
 where
  nextPos sp _ _             =  incSourceLine sp 1

anyString                    =  stringPrim Just

string s                     =  stringPrim $ (>> Just s) . guard . (== s)

initialChar c                =  stringPrim test 
 where
  test s@(c:_)               =  Just s
  test _                     =  Nothing

oneOf strings                =  choice $ map (try . string) strings

option                      ::  String -> [String] -> SParser String
option (s:hort) long         =  return [s]  <<  option' (s:hort) long  
option short (lon:g)         =  return lon  <<  option' short (lon:g)  
option [ ] [ ]               =  return [ ]

option' short long           =  do
  oneOf $ map (('-':) . (:[])) short ++ map ("--" ++) long

(<<)                         =  flip (>>)

type SParser res             =  GenParser String () res 

