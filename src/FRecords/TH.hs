{-# LANGUAGE TemplateHaskell #-}

module FRecords.TH
  ( makeFRecord
  ) where

import FRecords.Classes

import Language.Haskell.TH
import Language.Haskell.TH.Syntax

makeFRecord :: Name -> DecsQ
makeFRecord tyName = do
  info <- reify tyName
  case info of
    TyConI dec -> do
      genDec <- makeFRecordForDec dec
      let bothDecs = BothDecs { sourceDec = dec, generatedDec = genDec }
      ffunctorDecs <- makeFFunctorInstance bothDecs
      ftraverseDecs <- makeFTraversableInstance bothDecs
      pure $ [genDec] ++ ffunctorDecs ++ ftraverseDecs
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
    fFieldStrictness strictness =
      case strictness of
        IsStrict -> IsStrict
        Unpacked -> IsStrict
        NotStrict -> NotStrict

data BothDecs = BothDecs { sourceDec :: Dec, generatedDec :: Dec }

makeFFunctorInstance :: BothDecs -> DecsQ
makeFFunctorInstance bothDecs =
  case bothDecs of
    BothDecs (DataD _ _tyName tyVars _constrs _) (DataD _ fTyName _ fConstrs _) ->
      let tyVarNames = map nameFromTyVarBndr tyVars
      in
        [d|
          instance FFunctor $(mkS fTyName tyVarNames) where
            ffmap f rec =
              $(caseE [e| rec |] (map (ffmapConstrCase 'f) fConstrs))
        |]
    BothDecs (NewtypeD _ _tyName tyVars _constr _) (NewtypeD _ fTyName _ fConstr _) ->
      let tyVarNames = map nameFromTyVarBndr tyVars
      in
        [d|
          instance FFunctor $(mkS fTyName tyVarNames) where
            ffmap f rec =
              $(caseE [e| rec |] [ffmapConstrCase 'f fConstr])
        |]
    BothDecs source generated ->
      fail $ "makeFFunctorInstance is not implemented for " ++ show source ++
        " and " ++ show generated
  where
    -- | Generates `f arg1 ... argn`
    appEs :: ExpQ -> [ExpQ] -> ExpQ
    appEs = foldl appE
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

makeFTraversableInstance :: BothDecs -> DecsQ
makeFTraversableInstance bothDecs =
  case bothDecs of
    BothDecs (DataD _ _tyName tyVars _constrs _) (DataD _ fTyName _ fConstrs _) ->
      let tyVarNames = map nameFromTyVarBndr tyVars
      in
        [d|
          instance FTraversable $(mkS fTyName tyVarNames) where
            ftraverse f rec =
              $(caseE [e| rec |] (map (ftraverseConstrCase 'f) fConstrs))
        |]
    BothDecs (NewtypeD _ _tyName tyVars _constr _) (NewtypeD _ fTyName _ fConstr _) ->
      let tyVarNames = map nameFromTyVarBndr tyVars
      in
        [d|
          instance FTraversable $(mkS fTyName tyVarNames) where
            ftraverse f rec =
              $(caseE [e| rec |] [ftraverseConstrCase 'f fConstr])
        |]
    BothDecs source generated ->
      fail $ "makeFFunctorInstance is not implemented for " ++ show source ++
        " and " ++ show generated
  where
    mkS tyName vars = pure (tyName `conAppsT` map VarT vars)
    -- | Generates `f <*> arg1 <*> ... <*> argn`
    apAppEs :: ExpQ -> [ExpQ] -> ExpQ
    apAppEs = foldl (\g y -> [e| $g <*> $y |])
    -- | Generates `f <$> arg1 <*> ... <*> argn`
    liftAppEs :: ExpQ -> [ExpQ] -> ExpQ
    liftAppEs x args =
      case args of
        [] -> [e| pure $x |]
        firstArg:nextArgs -> apAppEs [e| $x <$> $firstArg |] nextArgs
    ftraverseMatch :: Name -> Name -> Int -> MatchQ
    ftraverseMatch funName constrName constrArity = do
      argNames <- mapM (\i -> newName ("x" ++ show i)) [1..constrArity]
      let pat = conP constrName (map varP argNames)
          mapArg v = [e| unCompose ($(varE funName) $(varE v)) |]
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

-- | Extract the name from a TyVarBndr.
nameFromTyVarBndr :: TyVarBndr -> Name
nameFromTyVarBndr bndr =
  case bndr of
    PlainTV name -> name
    KindedTV name _kind -> name

-- | Apply arguments to a type constructor.
conAppsT :: Name -> [Type] -> Type
conAppsT conName = foldl AppT (ConT conName)
