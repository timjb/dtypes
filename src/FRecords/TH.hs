module FRecords.TH
  ( makeFRecord
  ) where

--import FRecords.Classes

import Language.Haskell.TH
import Language.Haskell.TH.Syntax

makeFRecord :: Name -> DecsQ
makeFRecord tyName = do
  info <- reify tyName
  case info of
    TyConI dec -> makeFRecordForDec dec
    _ -> fail "makeFRecord: Expected type constructor name"

modifyName :: (String -> String) -> Name -> Name
modifyName f name =
  let Name (OccName str) flavour = name
  in Name (OccName (f str)) flavour

makeFRecordForDec :: Dec -> DecsQ
makeFRecordForDec dec =
  case dec of
    DataD ctx tyName tyVars constrs _deriving -> do
      (functorTyVarName, functorTyVarBndr) <- functorTyVar
      let fCtx = ctx
          fTyName = modifyName ("F" ++) tyName
          fTyVars = tyVars ++ [functorTyVarBndr]
      fConstrs <- mapM (makeFConForCon functorTyVarName) constrs
      return [DataD fCtx fTyName fTyVars fConstrs fDeriving]
    NewtypeD ctx tyName tyVars constr _deriving -> do
      (functorTyVarName, functorTyVarBndr) <- functorTyVar
      let fCtx = ctx
          fTyName = modifyName ("F" ++) tyName
          fTyVars = tyVars ++ [functorTyVarBndr]
      fConstr <- makeFConForCon functorTyVarName constr
      return [NewtypeD fCtx fTyName fTyVars fConstr fDeriving]
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

{-
  mkFunctor1' :: Dec -> Q [Dec]
  mkFunctor1' (DataD _ tnm pars cons _) =
    do names <- mapM newName $ map (('v':) . show) [1..(30 :: Int)]
       let (ps, f) = (\(x: xs) -> (reverse xs, x)) $ reverse $ map getTV pars
           parName = mkName "f"
           mkCons c =
             let (cnm, tps) = getCons c
                 (insts, exps) = unzip $ zipWith mkType names tps
             in ( concat insts
                , Clause
                    [VarP parName, ConP cnm $ map VarP $ take (length exps) names]
                    (NormalB $ foldl AppE (ConE cnm) exps)
                    []
                )
           mkType vnm (AppT tf a) | tf == VarT f = ([], VarE parName `AppE` VarE vnm)
                                  | a == VarT f  = ([tf], VarE (mkName "fmap1") `AppE` VarE parName `AppE` VarE vnm)
           mkType vnm _ = ([], VarE vnm)
           (preds, clauses) = first concat $ unzip $ map mkCons cons
       return [InstanceD
                   (map (ClassP (mkName "Functor1") . (:[])) preds)
                   (ConT (mkName "Functor1") `AppT` (foldl AppT (ConT tnm) $ map VarT ps))
                   [FunD (mkName "fmap1") clauses]
              ]
  mkFunctor1' _ = error "Can only derive Functor1 for data type"
-}