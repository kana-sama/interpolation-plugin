{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE StandaloneDeriving #-}

module Plugin where

import Data.Data (Data)
import Data.Generics.Uniplate.Data (transformBi)
import FastString (fsLit, unpackFS)
import GHC (GhcPs, HsExpr (ExplicitList, HsApp, HsLit, HsPar, HsVar), HsLit (HsString), LHsExpr, NoExtField (NoExtField), RdrName, noLoc)
import GhcPlugins (HsParsedModule (HsParsedModule), Plugin (parsedResultAction), RdrName, SourceText (NoSourceText), defaultPlugin, fsLit, mkRdrUnqual, mkVarOcc, noLoc, unpackFS)

deriving stock instance Data HsParsedModule

plugin :: Plugin
plugin = defaultPlugin {parsedResultAction = \_ _ -> pure . transformBi f}
  where
    f (HsLit _ (HsString _ str))
      | Just template <- viewTemplate (unpackFS str) =
        HsPar NoExtField (prettyTemplate template)
    f other = other

data TemplateItem = Str String | Var String

prettyTemplate :: [TemplateItem] -> LHsExpr GhcPs
prettyTemplate template =
  appE (varE (rdrName "mconcat")) (listE [ofItem i | i <- template])
  where
    ofItem (Str s) = stringE s
    ofItem (Var v) = varE (rdrName v)

    stringE s = noLoc (HsLit NoExtField (HsString NoSourceText (fsLit s))) :: LHsExpr GhcPs
    listE xs = noLoc (ExplicitList NoExtField Nothing xs) :: LHsExpr GhcPs
    varE x = noLoc (HsVar NoExtField (noLoc x)) :: LHsExpr GhcPs
    appE a b = noLoc (HsApp NoExtField a b) :: LHsExpr GhcPs
    rdrName = mkRdrUnqual . mkVarOcc

viewTemplate :: String -> Maybe [TemplateItem]
viewTemplate str =
  case optimize (parse str) of
    [] -> Nothing
    [Str x] -> Nothing
    template -> Just template
  where
    parse :: String -> [TemplateItem]
    parse ('\\' : '#' : '{' : str) = Str "\\#{" : parse str
    parse ('#' : '{' : str) =
      case break (== '}') str of
        ("", _) -> error "wtf"
        (var, '}' : str) -> Var var : parse str
        _ -> error "wtf"
    parse (c : str) = Str [c] : parse str
    parse [] = []

    optimize :: [TemplateItem] -> [TemplateItem]
    optimize (Str a : Str b : xs) = optimize (Str (a ++ b) : xs)
    optimize (i : xs) = i : optimize xs
    optimize [] = []
