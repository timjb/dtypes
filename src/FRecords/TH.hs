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
      functorTyVarName <- newName "f"
      let fCtx = ctx
          fTyName = modifyName ("F" ++) tyName
          fDeriving = []
          kindArrow from to = arrowK `appK` from `appK` to
          starToStarKind = starK `kindArrow` starK
          functorTyVarBndr = KindedTV functorTyVarName starToStarKind
          fTyVars = tyVars ++ [functorTyVarBndr]
      fConstrs <- mapM (makeFConForCon functorTyVarName) constrs
      return [DataD fCtx fTyName fTyVars fConstrs fDeriving]
    _ -> fail $ "makeFRecord not implemented for " ++ show dec

makeFConForCon :: Name -> Con -> ConQ
makeFConForCon functorTyVarName con =
  case con of
    NormalC conName argTypes -> do
      let fConName = modifyName ("F" ++) conName
      fArgTypes <- mapM makeFStrictType argTypes
      return (NormalC fConName fArgTypes)
    _ -> fail $ "makeFConForCon not implemented for " ++ show con
  where
    makeFStrictType (strictness, fieldType) = do
      let fStrictness = fFieldStrictnessAnnotation strictness
          fFieldType = (VarT functorTyVarName) `AppT` fieldType
      return (fStrictness, fFieldType)
    fFieldStrictnessAnnotation strictness =
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