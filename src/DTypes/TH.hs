{-# LANGUAGE CPP #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE ViewPatterns #-}

module DTypes.TH
  ( makeDType
  ) where

import Safe (initMay)

import Data.Functor.Identity (Identity (..))
import DTypes.Classes
import DTypes.Compose
import DTypes.Internal.TH.Helpers
import DTypes.Trafo

#if !MIN_VERSION_base(4,8,0)
import Control.Applicative (Applicative (..), (<$>))
#endif

import Language.Haskell.TH
import Language.Haskell.TH.Syntax

makeDType :: Name -> DecsQ
makeDType tyName = do
  info <- reify tyName
  case info of
    TyConI dec -> do
      genDec <- makeDTypeForDec dec
      origType <-
        case getSimpleTypeInfo dec of
          Just typeDecInfo -> return typeDecInfo
          Nothing -> fail "could not get original type dec info!"
      genType <-
        case getSimpleTypeInfo genDec of
          Just typeDecInfo -> return typeDecInfo
          Nothing -> fail "could not get generated type dec info!"
      ffunctorDecs <- makeDFunctorInstance genType
      ftraverseDecs <- makeDTraversableInstance genType
      fapplicativeDecs <-
          if canDeriveDApplicative genType
            then makeDApplicativeInstance genType
            else pure []
      fchoiceDecs <-
          if canDeriveDChoice genType
            then makeDChoiceInstance genType
            else pure []
      hasDTypeDecs <- makeHasDTypeInstance origType genType
      pure $
        [genDec] ++ ffunctorDecs ++ ftraverseDecs ++
        fapplicativeDecs ++ fchoiceDecs ++ hasDTypeDecs
    _ -> fail "makeDType: Expected type constructor name"

modifyName :: (String -> String) -> Name -> Name
modifyName f name =
  let Name (OccName str) flavour = name
  in Name (OccName (f str)) flavour

makeDTypeForDec :: Dec -> DecQ
makeDTypeForDec dec =
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
    _ -> fail $ "makeDType not implemented for " ++ show dec
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

data SimpleConstrInfo
  = SimpleConstrInfo
  { sci_name :: Name
  , sci_numArgs :: Int
  } deriving (Show)

getSimpleConstrInfo :: Con -> Maybe SimpleConstrInfo
getSimpleConstrInfo con =
  case con of
    NormalC name args ->
      Just $ SimpleConstrInfo { sci_name = name, sci_numArgs = length args }
    RecC name args ->
      Just $ SimpleConstrInfo { sci_name = name, sci_numArgs = length args }
    _ -> Nothing

data SimpleTypeInfo
  = SimpleTypeInfo
  { stdi_typeName :: Name
  , stdi_typeArgs :: [TyVarBndr]
  , stdi_constrs :: [SimpleConstrInfo]
  } deriving (Show)

getSimpleTypeInfo :: Dec -> Maybe SimpleTypeInfo
getSimpleTypeInfo typeDec =
  case typeDec of
#if MIN_VERSION_template_haskell(2,11,0)
    DataD _ tyName tyVarBndrs _ tyConstrs _ -> do
#else
    DataD _ tyName tyVarBndrs tyConstrs _ -> do
#endif
      simpleTyConstrs <- mapM getSimpleConstrInfo tyConstrs
      Just $
        SimpleTypeInfo
        { stdi_typeName = tyName
        , stdi_typeArgs = tyVarBndrs
        , stdi_constrs = simpleTyConstrs
        }
#if MIN_VERSION_template_haskell(2,11,0)
    NewtypeD _ tyName tyVarBndrs _ tyConstr _ -> do
#else
    NewtypeD _ tyName tyVarBndrs tyConstr _ -> do
#endif
      simpleTyConstr <- getSimpleConstrInfo tyConstr
      Just $
        SimpleTypeInfo
        { stdi_typeName = tyName
        , stdi_typeArgs = tyVarBndrs
        , stdi_constrs = [simpleTyConstr]
        }
    _ -> Nothing

makeDFunctorInstance :: SimpleTypeInfo -> DecsQ
makeDFunctorInstance typeDec =
  case typeDec of
    SimpleTypeInfo fTyName (initMay -> Just tyVars) fConstrs ->
      let tyVarNames = map nameFromTyVarBndr tyVars
      in
        [d|
          instance DFunctor $(mkS fTyName tyVarNames) where
            ffmap f rec =
              $(caseE [e| rec |] (map (ffmapConstrCase 'f) fConstrs))
        |]
    _ ->
      fail $ "makeDFunctorInstance is not implemented for " ++ show typeDec
  where
    mkS tyName vars = pure (tyName `conAppsT` map VarT vars)
    ffmapConstrCase :: Name -> SimpleConstrInfo -> MatchQ
    ffmapConstrCase funName (SimpleConstrInfo fConstrName constrArity) = do
      argNames <- mapM (\i -> newName ("x" ++ show i)) [1..constrArity]
      let pat = conP fConstrName (map varP argNames)
          body = conE fConstrName `appEs` map (\v -> varE funName `appE` varE v) argNames
      match pat (normalB body) []

makeDTraversableInstance :: SimpleTypeInfo -> DecsQ
makeDTraversableInstance typeDec =
  case typeDec of
    SimpleTypeInfo fTyName (initMay -> Just tyVars) fConstrs ->
      let tyVarNames = map nameFromTyVarBndr tyVars
      in
        [d|
          instance DTraversable $(mkS fTyName tyVarNames) where
            ftraverse f rec =
              $(caseE [e| rec |] (map (ftraverseConstrCase 'f) fConstrs))
        |]
    _ ->
      fail $ "makeDFunctorInstance is not implemented for " ++ show typeDec
  where
    mkS tyName vars = pure (tyName `conAppsT` map VarT vars)
    ftraverseConstrCase :: Name -> SimpleConstrInfo -> MatchQ
    ftraverseConstrCase funName (SimpleConstrInfo fConstrName constrArity) = do
      argNames <- mapM (\i -> newName ("x" ++ show i)) [1..constrArity]
      let pat = conP fConstrName (map varP argNames)
          mapArg v = [e| getCompose ($(varE funName) $(varE v)) |]
          body = conE fConstrName `liftAppEs` map mapArg argNames
      match pat (normalB body) []

isProductType :: SimpleTypeInfo -> Bool
isProductType typeInfo =
  case stdi_constrs typeInfo of
    [_] -> True
    _ -> False

canDeriveDApplicative :: SimpleTypeInfo -> Bool
canDeriveDApplicative = isProductType

makeDApplicativeInstance :: SimpleTypeInfo -> DecsQ
makeDApplicativeInstance genType =
  case genType of
    SimpleTypeInfo fTyName (initMay -> Just tyVars) [fConstr] ->
      let tyVarNames = map nameFromTyVarBndr tyVars
      in
        [d|
          instance DApplicative $(mkS fTyName tyVarNames) where
            fpure x = $(fpureConstr [e| x |] fConstr)
            f <<*>> rec = $(fapConstr [e| f |] [e| rec |] fConstr)
        |]
    _ ->
      fail $ "makeDApplicativeInstance is not implemented for " ++ show genType
  where
    mkS tyName vars = pure (tyName `conAppsT` map VarT vars)
    fpureConstr :: ExpQ -> SimpleConstrInfo -> ExpQ
    fpureConstr valExp (SimpleConstrInfo fConstrName constrArity) =
      conE fConstrName `appEs` replicate constrArity valExp
    fapConstr :: ExpQ -> ExpQ -> SimpleConstrInfo -> ExpQ
    fapConstr funRecExpr argRecExpr (SimpleConstrInfo fConstrName constrArity) = do
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

isSumType :: SimpleTypeInfo -> Bool
isSumType typeInfo =
  all hasOneArgument (stdi_constrs typeInfo)
  where
    hasOneArgument constr = sci_numArgs constr == 1

canDeriveDChoice :: SimpleTypeInfo -> Bool
canDeriveDChoice = isSumType

makeDChoiceInstance :: SimpleTypeInfo -> DecsQ
makeDChoiceInstance genType =
  case genType of
    SimpleTypeInfo fTyName (initMay -> Just tyVars) fConstrs ->
      let tyVarNames = map nameFromTyVarBndr tyVars
      in
        [d|
          instance DChoice $(mkS fTyName tyVarNames) where
            fchoose val =
              $(caseE [e| val |] (map fchooseConstr fConstrs))
        |]
    _ ->
      fail $ "makeDChoiceInstance is not implemented for " ++ show genType
  where
    mkS tyName vars = pure (tyName `conAppsT` map VarT vars)
    fchooseConstr :: SimpleConstrInfo -> MatchQ
    fchooseConstr simpleConstrInfo =
      case simpleConstrInfo of
        SimpleConstrInfo { sci_name = fConstrName, sci_numArgs = 1 } -> do
          argName <- newName "x"
          let pat = conP fConstrName [varP argName]
              body =
                [e|
                  case $(varE argName) of
                    LeftF l ->
                      Left ($(conE fConstrName) l)
                    RightF r ->
                      Right ($(conE fConstrName) r)
                |]
          match pat (normalB body) []
        _ ->
          fail $
            "fchoiceConstr not implemented for constructor with " ++
            show (sci_numArgs simpleConstrInfo) ++ " arguments"

makeHasDTypeInstance
  :: SimpleTypeInfo -- ^ the original datatype
  -> SimpleTypeInfo -- ^ the generated f datatype
  -> DecsQ
makeHasDTypeInstance origTypeDec genTypeDec =
  let SimpleTypeInfo tyName tyVars constrs = origTypeDec
      SimpleTypeInfo fTyName _ fConstrs = genTypeDec
      tyVarNames = map nameFromTyVarBndr tyVars
  in
    [d|
      instance HasDType $(mkS tyName tyVarNames) where
        type DType $(mkS tyName tyVarNames) = $(mkS fTyName tyVarNames)
        fiso rec =
          $(caseE [e| rec |] (zipWith fisoConstrCase constrs fConstrs))
        fosi frec =
          $(caseE [e| frec |] (zipWith fosiConstrCase constrs fConstrs))
    |]
  where
    mkS tyName vars = pure (tyName `conAppsT` map VarT vars)
    fisoConstrCase :: SimpleConstrInfo -> SimpleConstrInfo -> MatchQ
    fisoConstrCase origConstr genConstr = do
      let SimpleConstrInfo constrName constrArity = origConstr
          SimpleConstrInfo fConstrName _ = genConstr
      argNames <- mapM (\i -> newName ("x" ++ show i)) [1..constrArity]
      let pat = conP constrName (map varP argNames)
          body = conE fConstrName `appEs` map (\v -> [| Identity $(varE v) |]) argNames
      match pat (normalB body) []
    fosiConstrCase :: SimpleConstrInfo -> SimpleConstrInfo -> MatchQ
    fosiConstrCase origConstr genConstr = do
      let SimpleConstrInfo constrName constrArity = origConstr
          SimpleConstrInfo fConstrName _ = genConstr
      argNames <- mapM (\i -> newName ("x" ++ show i)) [1..constrArity]
      let pat = conP fConstrName (map (\v -> [p| Identity $(varP v) |]) argNames)
          body = conE constrName `appEs` map varE argNames
      match pat (normalB body) []
