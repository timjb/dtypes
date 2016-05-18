{-# LANGUAGE CPP #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE ViewPatterns #-}

module FRecords.TH
  ( makeFRecord
  ) where

import Safe (initMay)

import Data.Functor.Identity (Identity (..))
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
      hasFTypeDecs <- makeHasFTypeInstance dec genDec
      pure $ [genDec] ++ ffunctorDecs ++ ftraverseDecs ++ fapplicativeDecs ++ hasFTypeDecs
    _ -> fail "makeFRecord: Expected type constructor name"

modifyName :: (String -> String) -> Name -> Name
modifyName f name =
  let Name (OccName str) flavour = name
  in Name (OccName (f str)) flavour

makeFRecordForDec :: Dec -> DecQ
makeFRecordForDec dec =
  case dec of
#if MIN_VERSION_template_haskell(2,11,0)
    DataD ctx tyName tyVars _kind constrs _deriving -> do
#else
    DataD ctx tyName tyVars constrs _deriving -> do
#endif
      (functorTyVarName, functorTyVarBndr) <- functorTyVar
      let fCtx = ctx
          fTyName = modifyName ("F" ++) tyName
          fTyVars = tyVars ++ [functorTyVarBndr]
      fConstrs <- mapM (makeFConForCon functorTyVarName) constrs
#if MIN_VERSION_template_haskell(2,11,0)
      return (DataD fCtx fTyName fTyVars Nothing fConstrs fDeriving)
#else
      return (DataD fCtx fTyName fTyVars fConstrs fDeriving)
#endif
#if MIN_VERSION_template_haskell(2,11,0)
    NewtypeD ctx tyName tyVars _ constr _deriving -> do
#else
    NewtypeD ctx tyName tyVars constr _deriving -> do
#endif
      (functorTyVarName, functorTyVarBndr) <- functorTyVar
      let fCtx = ctx
          fTyName = modifyName ("F" ++) tyName
          fTyVars = tyVars ++ [functorTyVarBndr]
      fConstr <- makeFConForCon functorTyVarName constr
#if MIN_VERSION_template_haskell(2,11,0)
      return (NewtypeD fCtx fTyName fTyVars Nothing fConstr fDeriving)
#else
      return (NewtypeD fCtx fTyName fTyVars fConstr fDeriving)
#endif
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
    fFieldStrictness :: Bang -> Bang
    fFieldStrictness (Bang _unpackedness sourceStrictness) =
      Bang NoSourceUnpackedness sourceStrictness
#else
    fFieldStrictness :: Strict -> Strict
    fFieldStrictness strictness =
      case strictness of
        IsStrict -> IsStrict
        Unpacked -> IsStrict
        NotStrict -> NotStrict
#endif

data SimpleTypeDecInfo
  = SimpleTypeDecInfo
  { stdi_typeName :: Name
  , stdi_typeArgs :: [TyVarBndr]
  , stdi_constrs :: [Con]
  }

getSimpleTypeDecInfo :: Dec -> Maybe SimpleTypeDecInfo
getSimpleTypeDecInfo typeDec =
  case typeDec of
#if MIN_VERSION_template_haskell(2,11,0)
    DataD _ tyName tyVarBndrs _ tyConstrs _ ->
#else
    DataD _ tyName tyVarBndrs tyConstrs _ ->
#endif
      Just $
        SimpleTypeDecInfo
        { stdi_typeName = tyName
        , stdi_typeArgs = tyVarBndrs
        , stdi_constrs = tyConstrs
        }
#if MIN_VERSION_template_haskell(2,11,0)
    NewtypeD _ tyName tyVarBndrs _ tyConstr _ ->
#else
    NewtypeD _ tyName tyVarBndrs tyConstr _ ->
#endif
      Just $
        SimpleTypeDecInfo
        { stdi_typeName = tyName
        , stdi_typeArgs = tyVarBndrs
        , stdi_constrs = [tyConstr]
        }
    _ -> Nothing

data SimpleConstrInfo
  = SimpleConstrInfo
  { sci_name :: Name
  , sci_numArgs :: Int
  }

getSimpleConstrInfo :: Con -> Maybe SimpleConstrInfo
getSimpleConstrInfo con =
  case con of
    NormalC name args ->
      Just $ SimpleConstrInfo { sci_name = name, sci_numArgs = length args }
    RecC name args ->
      Just $ SimpleConstrInfo { sci_name = name, sci_numArgs = length args }
    _ -> Nothing

makeFFunctorInstance :: Dec -> DecsQ
makeFFunctorInstance typeDec =
  case getSimpleTypeDecInfo typeDec of
    Just (SimpleTypeDecInfo fTyName (initMay -> Just tyVars) fConstrs) ->
      let tyVarNames = map nameFromTyVarBndr tyVars
      in
        [d|
          instance FFunctor $(mkS fTyName tyVarNames) where
            ffmap f rec =
              $(caseE [e| rec |] (map (ffmapConstrCase 'f) fConstrs))
        |]
    _ ->
      fail $ "makeFFunctorInstance is not implemented for " ++ show typeDec
  where
    mkS tyName vars = pure (tyName `conAppsT` map VarT vars)
    ffmapConstrCase :: Name -> Con -> MatchQ
    ffmapConstrCase funName constr =
      case getSimpleConstrInfo constr of
        Just (SimpleConstrInfo fConstrName constrArity) -> do
          argNames <- mapM (\i -> newName ("x" ++ show i)) [1..constrArity]
          let pat = conP fConstrName (map varP argNames)
              body = conE fConstrName `appEs` map (\v -> varE funName `appE` varE v) argNames
          match pat (normalB body) []
        Nothing ->
          fail $ "ffmapConstrCase does not support " ++ show constr

makeFTraversableInstance :: Dec -> DecsQ
makeFTraversableInstance typeDec =
  case getSimpleTypeDecInfo typeDec of
    Just (SimpleTypeDecInfo fTyName (initMay -> Just tyVars) fConstrs) ->
      let tyVarNames = map nameFromTyVarBndr tyVars
      in
        [d|
          instance FTraversable $(mkS fTyName tyVarNames) where
            ftraverse f rec =
              $(caseE [e| rec |] (map (ftraverseConstrCase 'f) fConstrs))
        |]
    _ ->
      fail $ "makeFFunctorInstance is not implemented for " ++ show typeDec
  where
    mkS tyName vars = pure (tyName `conAppsT` map VarT vars)
    ftraverseConstrCase :: Name -> Con -> MatchQ
    ftraverseConstrCase funName constr =
      case getSimpleConstrInfo constr of
        Just (SimpleConstrInfo fConstrName constrArity) -> do
          argNames <- mapM (\i -> newName ("x" ++ show i)) [1..constrArity]
          let pat = conP fConstrName (map varP argNames)
              mapArg v = [e| getCompose ($(varE funName) $(varE v)) |]
              body = conE fConstrName `liftAppEs` map mapArg argNames
          match pat (normalB body) []
        Nothing ->
          fail $ "ftraverseConstrCase does not support " ++ show constr

canDeriveFApplicative :: Dec -> Bool
canDeriveFApplicative typeDec =
#if MIN_VERSION_template_haskell(2,11,0)
  case typeDec of
    NewtypeD _ _ _ _ _ _ -> True
    DataD _ _ _ _ [_constr] _ -> True
    _ -> False
#else
  case typeDec of
    NewtypeD _ _ _ _ _ -> True
    DataD _ _ _ [_constr] _ -> True
    _ -> False
#endif

makeFApplicativeInstance :: Dec -> DecsQ
makeFApplicativeInstance typeDec =
  case getSimpleTypeDecInfo typeDec of
    Just (SimpleTypeDecInfo fTyName (initMay -> Just tyVars) [fConstr]) ->
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
    fpureConstr :: ExpQ -> Con -> ExpQ
    fpureConstr valExp constr =
      case getSimpleConstrInfo constr of
        Just (SimpleConstrInfo fConstrName constrArity) ->
          conE fConstrName `appEs` replicate constrArity valExp
        Nothing ->
          fail $ "fpureConstr does not support " ++ show constr
    fapConstr :: ExpQ -> ExpQ -> Con -> ExpQ
    fapConstr funRecExpr argRecExpr constr =
      case getSimpleConstrInfo constr of
        Just (SimpleConstrInfo fConstrName constrArity) -> do
          let makeArgs prefix =
                mapM (\i -> newName (prefix ++ show i)) [1..constrArity]
          funArgNames <- makeArgs "f"
          argArgNames <- makeArgs "x"
          let funPat = conP fConstrName (map varP funArgNames)
              argPat = conP fConstrName (map varP argArgNames)
              applyComponent funName argName =
                [e| $(varE funName) $$ $(varE argName) |]
          destructure funRecExpr funPat $
            destructure argRecExpr argPat $
            conE fConstrName `appEs` zipWith applyComponent funArgNames argArgNames
        Nothing -> fail $ "fapConstr does not support " ++ show constr

makeHasFTypeInstance
  :: Dec -- ^ the original datatype
  -> Dec -- ^ the generated f datatype
  -> DecsQ
makeHasFTypeInstance origTypeDec genTypeDec =
  case (,) <$> getSimpleTypeDecInfo origTypeDec <*> getSimpleTypeDecInfo genTypeDec of
    Just (SimpleTypeDecInfo tyName tyVars constrs, SimpleTypeDecInfo fTyName _ fConstrs) ->
      let tyVarNames = map nameFromTyVarBndr tyVars
      in
        [d|
          instance HasFType $(mkS tyName tyVarNames) where
            type FType $(mkS tyName tyVarNames) = $(mkS fTyName tyVarNames)
            fiso rec =
              $(caseE [e| rec |] (zipWith fisoConstrCase constrs fConstrs))
            fosi frec =
              $(caseE [e| frec |] (zipWith fosiConstrCase constrs fConstrs))
        |]
    Nothing ->
      fail $ "makeHasFTypeInstance is not implemented for " ++ show genTypeDec
  where
    mkS tyName vars = pure (tyName `conAppsT` map VarT vars)
    fisoConstrCase :: Con -> Con -> MatchQ
    fisoConstrCase origConstr genConstr =
      case (,) <$> getSimpleConstrInfo origConstr <*> getSimpleConstrInfo genConstr of
        Just (SimpleConstrInfo constrName constrArity, SimpleConstrInfo fConstrName _) -> do
          argNames <- mapM (\i -> newName ("x" ++ show i)) [1..constrArity]
          let pat = conP constrName (map varP argNames)
              body = conE fConstrName `appEs` map (\v -> [| Identity $(varE v) |]) argNames
          match pat (normalB body) []
        Nothing ->
          fail $ "fisoConstrCase is not implemented for " ++ show genConstr
    fosiConstrCase :: Con -> Con -> MatchQ
    fosiConstrCase origConstr genConstr =
      case (,) <$> getSimpleConstrInfo origConstr <*> getSimpleConstrInfo genConstr of
        Just (SimpleConstrInfo constrName constrArity, SimpleConstrInfo fConstrName _) -> do
          argNames <- mapM (\i -> newName ("x" ++ show i)) [1..constrArity]
          let pat = conP fConstrName (map (\v -> [p| Identity $(varP v) |]) argNames)
              body = conE constrName `appEs` map varE argNames
          match pat (normalB body) []
        Nothing ->
          fail $ "fosiConstrCase is not implemented for " ++ show genConstr
