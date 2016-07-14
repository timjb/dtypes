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
      dfunctorDecs <- makeDFunctorInstance genType
      dtraverseDecs <- makeDTraversableInstance genType
      dapplicativeDecs <-
          if canDeriveDApplicative genType
            then makeDApplicativeInstance genType
            else pure []
      dchoiceDecs <-
          if canDeriveDChoice genType
            then makeDChoiceInstance genType
            else pure []
      hasDTypeDecs <- makeHasDTypeInstance origType genType
      pure $
        [genDec] ++ dfunctorDecs ++ dtraverseDecs ++
        dapplicativeDecs ++ dchoiceDecs ++ hasDTypeDecs
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
      let dCtx = ctx
          dTyName = modifyName ("D" ++) tyName
          dTyVars = tyVars ++ [functorTyVarBndr]
      dConstrs <- mapM (makeFConForCon functorTyVarName) constrs
#if MIN_VERSION_template_haskell(2,11,0)
      return (DataD dCtx dTyName dTyVars Nothing dConstrs fDeriving)
#else
      return (DataD dCtx dTyName dTyVars dConstrs fDeriving)
#endif
#if MIN_VERSION_template_haskell(2,11,0)
    NewtypeD ctx tyName tyVars _ constr _deriving -> do
#else
    NewtypeD ctx tyName tyVars constr _deriving -> do
#endif
      (functorTyVarName, functorTyVarBndr) <- functorTyVar
      let dCtx = ctx
          dTyName = modifyName ("D" ++) tyName
          dTyVars = tyVars ++ [functorTyVarBndr]
      dConstr <- makeFConForCon functorTyVarName constr
#if MIN_VERSION_template_haskell(2,11,0)
      return (NewtypeD dCtx dTyName dTyVars Nothing dConstr fDeriving)
#else
      return (NewtypeD dCtx dTyName dTyVars dConstr fDeriving)
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
      let dConName = modifyName ("D" ++) conName
      dArgTypes <- mapM makeDStrictType argTypes
      return (NormalC dConName dArgTypes)
    RecC conName argTypes -> do
      let dConName = modifyName ("D" ++) conName
      dArgTypes <- mapM makeDVarStrictType argTypes
      return (RecC dConName dArgTypes)
    _ -> fail $ "makeFConForCon not implemented for " ++ show con
  where
    makeDStrictType (strictness, ty) = do
      return (dFieldStrictness strictness, dFieldType ty)
    makeDVarStrictType (fieldName, strictness, ty) = do
      return (dFieldName fieldName, dFieldStrictness strictness, dFieldType ty)
    dFieldName = modifyName ("d" ++)
    dFieldType ty = (VarT functorTyVarName) `AppT` ty
#if MIN_VERSION_template_haskell(2,11,0)
    dFieldStrictness :: Bang -> Bang
    dFieldStrictness (Bang _unpackedness sourceStrictness) =
      Bang NoSourceUnpackedness sourceStrictness
#else
    dFieldStrictness :: Strict -> Strict
    dFieldStrictness strictness =
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
    SimpleTypeInfo dTyName (initMay -> Just tyVars) dConstrs ->
      let tyVarNames = map nameFromTyVarBndr tyVars
      in
        [d|
          instance DFunctor $(mkS dTyName tyVarNames) where
            dfmap f rec =
              $(caseE [e| rec |] (map (dfmapConstrCase 'f) dConstrs))
        |]
    _ ->
      fail $ "makeDFunctorInstance is not implemented for " ++ show typeDec
  where
    mkS tyName vars = pure (tyName `conAppsT` map VarT vars)
    dfmapConstrCase :: Name -> SimpleConstrInfo -> MatchQ
    dfmapConstrCase funName (SimpleConstrInfo dConstrName constrArity) = do
      argNames <- mapM (\i -> newName ("x" ++ show i)) [1..constrArity]
      let pat = conP dConstrName (map varP argNames)
          body = conE dConstrName `appEs` map (\v -> varE funName `appE` varE v) argNames
      match pat (normalB body) []

makeDTraversableInstance :: SimpleTypeInfo -> DecsQ
makeDTraversableInstance typeDec =
  case typeDec of
    SimpleTypeInfo dTyName (initMay -> Just tyVars) dConstrs ->
      let tyVarNames = map nameFromTyVarBndr tyVars
      in
        [d|
          instance DTraversable $(mkS dTyName tyVarNames) where
            dtraverse f rec =
              $(caseE [e| rec |] (map (dtraverseConstrCase 'f) dConstrs))
        |]
    _ ->
      fail $ "makeDFunctorInstance is not implemented for " ++ show typeDec
  where
    mkS tyName vars = pure (tyName `conAppsT` map VarT vars)
    dtraverseConstrCase :: Name -> SimpleConstrInfo -> MatchQ
    dtraverseConstrCase funName (SimpleConstrInfo dConstrName constrArity) = do
      argNames <- mapM (\i -> newName ("x" ++ show i)) [1..constrArity]
      let pat = conP dConstrName (map varP argNames)
          mapArg v = [e| getCompose ($(varE funName) $(varE v)) |]
          body = conE dConstrName `liftAppEs` map mapArg argNames
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
    SimpleTypeInfo dTyName (initMay -> Just tyVars) [dConstr] ->
      let tyVarNames = map nameFromTyVarBndr tyVars
      in
        [d|
          instance DApplicative $(mkS dTyName tyVarNames) where
            dpure x = $(dpureConstr [e| x |] dConstr)
            f <<*>> rec = $(dapConstr [e| f |] [e| rec |] dConstr)
        |]
    _ ->
      fail $ "makeDApplicativeInstance is not implemented for " ++ show genType
  where
    mkS tyName vars = pure (tyName `conAppsT` map VarT vars)
    dpureConstr :: ExpQ -> SimpleConstrInfo -> ExpQ
    dpureConstr valExp (SimpleConstrInfo dConstrName constrArity) =
      conE dConstrName `appEs` replicate constrArity valExp
    dapConstr :: ExpQ -> ExpQ -> SimpleConstrInfo -> ExpQ
    dapConstr funRecExpr argRecExpr (SimpleConstrInfo dConstrName constrArity) = do
      let makeArgs prefix =
            mapM (\i -> newName (prefix ++ show i)) [1..constrArity]
      funArgNames <- makeArgs "f"
      argArgNames <- makeArgs "x"
      let funPat = conP dConstrName (map varP funArgNames)
          argPat = conP dConstrName (map varP argArgNames)
          applyComponent funName argName =
            [e| $(varE funName) $$ $(varE argName) |]
      destructure funRecExpr funPat $
        destructure argRecExpr argPat $
        conE dConstrName `appEs` zipWith applyComponent funArgNames argArgNames

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
    SimpleTypeInfo dTyName (initMay -> Just tyVars) dConstrs ->
      let tyVarNames = map nameFromTyVarBndr tyVars
      in
        [d|
          instance DChoice $(mkS dTyName tyVarNames) where
            dchoose val =
              $(caseE [e| val |] (map dchooseConstr dConstrs))
        |]
    _ ->
      fail $ "makeDChoiceInstance is not implemented for " ++ show genType
  where
    mkS tyName vars = pure (tyName `conAppsT` map VarT vars)
    dchooseConstr :: SimpleConstrInfo -> MatchQ
    dchooseConstr simpleConstrInfo =
      case simpleConstrInfo of
        SimpleConstrInfo { sci_name = dConstrName, sci_numArgs = 1 } -> do
          argName <- newName "x"
          let pat = conP dConstrName [varP argName]
              body =
                [e|
                  case $(varE argName) of
                    LeftF l ->
                      Left ($(conE dConstrName) l)
                    RightF r ->
                      Right ($(conE dConstrName) r)
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
      SimpleTypeInfo dTyName _ dConstrs = genTypeDec
      tyVarNames = map nameFromTyVarBndr tyVars
  in
    [d|
      instance HasDType $(mkS tyName tyVarNames) where
        type DType $(mkS tyName tyVarNames) = $(mkS dTyName tyVarNames)
        diso rec =
          $(caseE [e| rec |] (zipWith disoConstrCase constrs dConstrs))
        dosi frec =
          $(caseE [e| frec |] (zipWith dosiConstrCase constrs dConstrs))
    |]
  where
    mkS tyName vars = pure (tyName `conAppsT` map VarT vars)
    disoConstrCase :: SimpleConstrInfo -> SimpleConstrInfo -> MatchQ
    disoConstrCase origConstr genConstr = do
      let SimpleConstrInfo constrName constrArity = origConstr
          SimpleConstrInfo dConstrName _ = genConstr
      argNames <- mapM (\i -> newName ("x" ++ show i)) [1..constrArity]
      let pat = conP constrName (map varP argNames)
          body = conE dConstrName `appEs` map (\v -> [| Identity $(varE v) |]) argNames
      match pat (normalB body) []
    dosiConstrCase :: SimpleConstrInfo -> SimpleConstrInfo -> MatchQ
    dosiConstrCase origConstr genConstr = do
      let SimpleConstrInfo constrName constrArity = origConstr
          SimpleConstrInfo dConstrName _ = genConstr
      argNames <- mapM (\i -> newName ("x" ++ show i)) [1..constrArity]
      let pat = conP dConstrName (map (\v -> [p| Identity $(varP v) |]) argNames)
          body = conE constrName `appEs` map varE argNames
      match pat (normalB body) []
