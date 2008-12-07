{- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -

  SYNOPSIS
    hooty (-1|-4)? (-n <number to make>)?

  DESCRIPTION

    The `hooty` program generates any number of UUIDs (one by default), using
    either the version 1 (time and MAC) or version 4 (random) algorithm
    (version 1 is the default). On all platforms, `hooty` uses the native
    implementation.

  OPTIONS

    -n, --number <number>
        Create such-and-such many UUIDs in one go.

    -1, --sequential
        Create UUIDs using the version 1 (time and MAC) algorithm.

    -4, --random
        Create UUIDs using the version 4 (random) algorithm.

    -h, -?, --help
        Print this help and exit.

    --version
        Print version and exit.

 - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -}

{-# LANGUAGE TemplateHaskell
           , PatternGuards
  #-}

import qualified System.UUID.V1 as V1
import qualified System.UUID.V4 as V4
import Options
import Messages 
import qualified Macros as Macros
import System.Environment
import System.Exit
import Control.Monad
import Control.Applicative
import Data.Maybe
import Data.Word
import qualified Data.Map as Map

main                         =  do
  m                         <-  opts
  let
    lk                       =  (`Map.lookup` m)
  when (isJust $ lk "h") $ do
    stdout << usage
    exitWith ExitSuccess
  when (isJust $ lk "version") $ do
    stdout << version
    exitWith ExitSuccess
  when (all (isJust . lk) ["1","4"]) $ do
    bail "Please specify either version 1 or version 4, not both."
  let
    n                       ::  Word
    n                        =  fromMaybe 1 $ maybeRead =<< lk "n"
    gen =
      if isJust $ lk "4"
        then  V4.uuid
        else  V1.uuid
  mapM_ (const $ print =<< gen) [1..n]


bail                        ::  String -> IO a
bail s                       =  do 
  stderr << s
  stderr << usage
  exitFailure


usage                        =  $(Macros.usage)

version                      =  "hooty-" ++ $(Macros.version)


opts                         =  do
  args                      <-  getArgs
  case runParser options () "command line arguments" args of
    Right list              ->  return $ foldr ($) Map.empty list
    Left e                  ->  bail $ show e


options                      =  do
  res                       <-  choice
    [ eof >> return []
    , many1 options'
    ]
  eof
  return res


options'                     =  do
  o                         <-  choice opts
  opt o
 where
  opt o@[c]
    | c `elem` "h14"         =  return $ Map.insert o ""
    | c == 'n'               =  choice
          [ eof >> fail "Option requires an argument."
          , try $ do
              s             <-  initialChar '-'
              fail $ "Option requiring argument followed by:\n  " ++ s
          , fmap (Map.insert o) anyString
          ]
    | otherwise              =  prb $ "unimplemented option '" ++ o ++ "'"
  opt "version"              =  return $ Map.insert "version" ""
  opt o                      =  prb $ "unimplemented option '" ++ o ++ "'"
  prb s                      =  fail $ "Please report a bug -- " ++ s ++ "."
  opts                       =  map try
    [ option    "h?"            ["help"]
    , option    "1"             ["sequential"]
    , option    "4"             ["random"]
    , option    "n"             ["number"]
    , option    ""              ["version"]
    ] ++ [ fail "Invalid option." ]


maybeRead s
  | [(a, _)]     <- reads s  =  Just a
  | otherwise                =  Nothing

