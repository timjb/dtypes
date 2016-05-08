{-# LANGUAGE CPP #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE ViewPatterns #-}

module FRecords.TH
  ( makeFRecord
  ) where

import Safe (initMay)

import FRecords.Classes
import FRecords.Internal.TH.Helpers

#if !MIN_VERSION_base(4,8,0)
import Control.Applicative (Applicative (..), (<$>))
#endif

import Language.Haskell.TH
import Language.Haskell.TH.Syntax

makeFRecord :: Name -> DecsQ
makeFRecord tyName = do
  info <- reify tyName
  case info of
    TyConI dec -> do
      genDec <- makeFRecordForDec dec
      ffunctorDecs <- makeFFunctorInstance genDec
      ftraverseDecs <- makeFTraversableInstance genDec
      fapplicativeDecs <-
          if canDeriveFApplicative genDec
            then makeFApplicativeInstance genDec
            else pure []
      pure $ [genDec] ++ ffunctorDecs ++ ftraverseDecs ++ fapplicativeDecs
    _ -> fail "makeFRecord: Expected type constructor name"

modifyName :: (String -> String) -> Name -> Name
modifyName f name =
  let Name (OccName str) flavour = name
  in Name (OccName (f str)) flavour

makeFRecordForDec :: Dec -> DecQ
makeFRecordForDec dec =
  case dec of
    DataD ctx tyName tyVars constrs _deriving -> do
      (functorTyVarName, functorTyVarBndr) <- functorTyVar
      let fCtx = ctx
          fTyName = modifyName ("F" ++) tyName
          fTyVars = tyVars ++ [functorTyVarBndr]
      fConstrs <- mapM (makeFConForCon functorTyVarName) constrs
      return (DataD fCtx fTyName fTyVars fConstrs fDeriving)
    NewtypeD ctx tyName tyVars constr _deriving -> do
      (functorTyVarName, functorTyVarBndr) <- functorTyVar
      let fCtx = ctx
          fTyName = modifyName ("F" ++) tyName
          fTyVars = tyVars ++ [functorTyVarBndr]
      fConstr <- makeFConForCon functorTyVarName constr
      return (NewtypeD fCtx fTyName fTyVars fConstr fDeriving)
    _ -> fail $ "makeFRecord not implemented for " ++ show dec
  where
    fDeriving = []
    functorTyVar = do
      functorTyVarName <- newName "f"
      let kindArrow from to = arrowK `appK` from `appK` to
          starToStarKind = starK `kindArrow` starK
          functorTyVarBndr = KindedTV functorTyVarName starToStarKind
      return (functorTyVarName, functorTyVarBndr)

makeFConForCon :: Name -> Con -> ConQ
makeFConForCon functorTyVarName con =
  case con of
    NormalC conName argTypes -> do
      let fConName = modifyName ("F" ++) conName
      fArgTypes <- mapM makeFStrictType argTypes
      return (NormalC fConName fArgTypes)
    RecC conName argTypes -> do
      let fConName = modifyName ("F" ++) conName
      fArgTypes <- mapM makeFVarStrictType argTypes
      return (RecC fConName fArgTypes)
    _ -> fail $ "makeFConForCon not implemented for " ++ show con
  where
    makeFStrictType (strictness, ty) = do
      return (fFieldStrictness strictness, fFieldType ty)
    makeFVarStrictType (fieldName, strictness, ty) = do
      return (fFieldName fieldName, fFieldStrictness strictness, fFieldType ty)
    fFieldName = modifyName ("f" ++)
    fFieldType ty = (VarT functorTyVarName) `AppT` ty
    #if MIN_VERSION_template_haskell(2,11,0)
    fFieldStrictness strictness = _
    #else
    fFieldStrictness strictness =
      case strictness of
        IsStrict -> IsStrict
        Unpacked -> IsStrict
        NotStrict -> NotStrict
    #endif

makeFFunctorInstance :: Dec -> DecsQ
makeFFunctorInstance typeDec =
  case typeDec of
    DataD _ fTyName (initMay -> Just tyVars) fConstrs _ ->
      let tyVarNames = map nameFromTyVarBndr tyVars
      in
        [d|
          instance FFunctor $(mkS fTyName tyVarNames) where
            ffmap f rec =
              $(caseE [e| rec |] (map (ffmapConstrCase 'f) fConstrs))
        |]
    NewtypeD _ fTyName (initMay -> Just tyVars) fConstr _ ->
      let tyVarNames = map nameFromTyVarBndr tyVars
      in
        [d|
          instance FFunctor $(mkS fTyName tyVarNames) where
            ffmap f rec =
              $(caseE [e| rec |] [ffmapConstrCase 'f fConstr])
        |]
    _ -> fail $ "makeFFunctorInstance is not implemented for " ++ show typeDec
  where
    mkS tyName vars = pure (tyName `conAppsT` map VarT vars)
    ffmapMatch :: Name -> Name -> Int -> MatchQ
    ffmapMatch funName constrName constrArity = do
      argNames <- mapM (\i -> newName ("x" ++ show i)) [1..constrArity]
      let pat = conP constrName (map varP argNames)
          body = conE constrName `appEs` map (\v -> varE funName `appE` varE v) argNames
      match pat (normalB body) []
    ffmapConstrCase :: Name -> Con -> MatchQ
    ffmapConstrCase funName constr =
      case constr of
        NormalC fConName fArgTypes ->
          ffmapMatch funName fConName (length fArgTypes)
        RecC fConName fArgTypes ->
          ffmapMatch funName fConName (length fArgTypes)
        _ -> fail $ "ffmapConstrCase does not support " ++ show constr

makeFTraversableInstance :: Dec -> DecsQ
makeFTraversableInstance typeDec =
  case typeDec of
    DataD _ fTyName (initMay -> Just tyVars) fConstrs _ ->
      let tyVarNames = map nameFromTyVarBndr tyVars
      in
        [d|
          instance FTraversable $(mkS fTyName tyVarNames) where
            ftraverse f rec =
              $(caseE [e| rec |] (map (ftraverseConstrCase 'f) fConstrs))
        |]
    NewtypeD _ fTyName (initMay -> Just tyVars) fConstr _ ->
      let tyVarNames = map nameFromTyVarBndr tyVars
      in
        [d|
          instance FTraversable $(mkS fTyName tyVarNames) where
            ftraverse f rec =
              $(caseE [e| rec |] [ftraverseConstrCase 'f fConstr])
        |]
    _ -> fail $ "makeFFunctorInstance is not implemented for " ++ show typeDec
  where
    mkS tyName vars = pure (tyName `conAppsT` map VarT vars)
    ftraverseMatch :: Name -> Name -> Int -> MatchQ
    ftraverseMatch funName constrName constrArity = do
      argNames <- mapM (\i -> newName ("x" ++ show i)) [1..constrArity]
      let pat = conP constrName (map varP argNames)
          mapArg v = [e| getCompose ($(varE funName) $(varE v)) |]
          body = conE constrName `liftAppEs` map mapArg argNames
      match pat (normalB body) []
    ftraverseConstrCase :: Name -> Con -> MatchQ
    ftraverseConstrCase funName constr =
      case constr of
        NormalC fConName fArgTypes ->
          ftraverseMatch funName fConName (length fArgTypes)
        RecC fConName fArgTypes ->
          ftraverseMatch funName fConName (length fArgTypes)
        _ -> fail $ "ftraverseConstrCase does not support " ++ show constr

canDeriveFApplicative :: Dec -> Bool
canDeriveFApplicative typeDec =
  case typeDec of
    NewtypeD _ _ _ _ _ -> True
    DataD _ _ _ [_constr] _ -> True
    _ -> False

makeFApplicativeInstance :: Dec -> DecsQ
makeFApplicativeInstance typeDec =
  case typeDec of
    DataD _ fTyName (initMay -> Just tyVars) [fConstr] _ ->
      let tyVarNames = map nameFromTyVarBndr tyVars
      in
        [d|
          instance FApplicative $(mkS fTyName tyVarNames) where
            fpure x = $(fpureConstr [e| x |] fConstr)
            f <<*>> rec = $(fapConstr [e| f |] [e| rec |] fConstr)
        |]
    NewtypeD _ fTyName (initMay -> Just tyVars) fConstr _ ->
      let tyVarNames = map nameFromTyVarBndr tyVars
      in
        [d|
          instance FApplicative $(mkS fTyName tyVarNames) where
            fpure x = $(fpureConstr [e| x |] fConstr)
            f <<*>> rec = $(fapConstr [e| f |] [e| rec |] fConstr)
        |]
    _ ->
      fail $ "makeFApplicativeInstance is not implemented for " ++ show typeDec
  where
    mkS tyName vars = pure (tyName `conAppsT` map VarT vars)
    fpureConstrExpr :: ExpQ -> Name -> Int -> ExpQ
    fpureConstrExpr valExp constrName constrArity =
      conE constrName `appEs` replicate constrArity valExp
    fpureConstr :: ExpQ -> Con -> ExpQ
    fpureConstr valExp constr =
      case constr of
        NormalC fConName fArgTypes ->
          fpureConstrExpr valExp fConName (length fArgTypes)
        RecC fConName fArgTypes ->
          fpureConstrExpr valExp fConName (length fArgTypes)
        _ -> fail $ "fpureConstr does not support " ++ show constr
    fapConstrExpr :: ExpQ -> ExpQ -> Name -> Int -> ExpQ
    fapConstrExpr funRecExpr argRecExpr constrName constrArity = do
      let makeArgs prefix =
            mapM (\i -> newName (prefix ++ show i)) [1..constrArity]
      funArgNames <- makeArgs "f"
      argArgNames <- makeArgs "x"
      let funPat = conP constrName (map varP funArgNames)
          argPat = conP constrName (map varP argArgNames)
          applyComponent funName argName =
            [e| $(varE funName) $$ $(varE argName) |]
      destructure funRecExpr funPat $
        destructure argRecExpr argPat $
        conE constrName `appEs` zipWith applyComponent funArgNames argArgNames
    fapConstr :: ExpQ -> ExpQ -> Con -> ExpQ
    fapConstr funRecExpr argRecExpr constr =
      case constr of
        NormalC fConName fArgTypes ->
          fapConstrExpr funRecExpr argRecExpr fConName (length fArgTypes)
        RecC fConName fArgTypes ->
          fapConstrExpr funRecExpr argRecExpr fConName (length fArgTypes)
        _ -> fail $ "fapConstr does not support " ++ show constr
