
{-| Macros for macros. 
 - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -}

{-# LANGUAGE TemplateHaskell
  #-}

module MacroMacros where


import Control.Applicative
import Language.Haskell.TH
import Language.Haskell.TH.Syntax



presentFile                  =  recover newTH oldTH
 where
  oldTH                      =  do
    reify name 
    return $ InfixE
      (Just $ VarE $ mkName "fileName")
      (VarE $ mkName "<$>")
      (Just $ VarE name)
   where
    name                     =  mkName "currentModule"
  newTH                      =  do 
    return $ InfixE
      (Just $ VarE $ mkName "loc_filename")
      (VarE $ mkName "<$>")
      (Just $ VarE $ mkName "location")


fileName mod                 =  map replace mod ++ ".hs"
 where
  replace '.'                =  '/'
  replace c                  =  c

