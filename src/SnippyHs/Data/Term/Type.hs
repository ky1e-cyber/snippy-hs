{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveFoldable #-}
{-# LANGUAGE DeriveFunctor #-}

module SnippyHs.Data.Term.Type
  ( Type (..),
    kindOfM,
    wellKindedM,
    validM,
    instantiateVarM,
    constCtr,
    varCtr,
    apCtr,
    funCtr,
    allTypeCtrs,
    voidType,
    kindFromConstUnchecked,
    kindFromVarUnchecked,
    literalTypes,
    eqTypeModNums,
    fromTFunUnchecked,
  )
where

import Data.Data (Constr, Data, dataTypeConstrs, dataTypeOf, toConstr)
import Data.Functor (($>))
import SnippyHs.Data.Term.Kind (Kind (..), applyKindM)

data Type i
  = TConst Kind String
  | TVar Kind i
  | TAp (Type i) (Type i)
  | TFun (Type i) (Type i)
  deriving (Functor, Foldable, Eq, Data)

literalTypes :: [Type i]
literalTypes =
  [ TConst Star "Double",
    TConst Star "Int",
    TConst Star "Char",
    TConst Star "String"
  ]

fromTFunUnchecked :: Type i -> (Type i, Type i)
fromTFunUnchecked (TFun t1 t2) = (t1, t2)
fromTFunUnchecked _ = error "fromTFunUnchecked failed: not TFun"

constCtr :: Constr
constCtr = toConstr (TConst Star "" :: Type ())

varCtr :: Constr
varCtr = toConstr (TVar Star ())

apCtr :: Constr
apCtr = toConstr (TAp (TVar Star ()) (TVar Star ()))

funCtr :: Constr
funCtr = toConstr (TFun (TVar Star ()) (TVar Star ()))

allTypeCtrs :: [Constr]
allTypeCtrs =
  dataTypeConstrs (dataTypeOf (TVar Star (42 :: Int)))

voidType :: Type i -> Type ()
voidType t = t $> ()

kindFromConstUnchecked :: Type i -> Kind
kindFromConstUnchecked (TConst k _) = k
kindFromConstUnchecked _ = error "kindFromConstUnchecked failed: not TConst"

kindFromVarUnchecked :: Type i -> Kind
kindFromVarUnchecked (TVar k _) = k
kindFromVarUnchecked _ = error "kindFromConstUnchecked failed: not TVar"

instance (Show i) => Show (Type i) where
  show (TConst _ name) = name
  show (TVar _ varId) = "$t_" ++ show varId
  show (TAp t1 t2) =
    "(" ++ show t1 ++ " " ++ show t2 ++ ")"
  show (TFun t1 t2) =
    "(" ++ show t1 ++ " -> " ++ show t2 ++ ")"

kindOfM :: (MonadFail m) => Type i -> m Kind
kindOfM (TConst k _) = pure k
kindOfM (TVar k _) = pure k
kindOfM (TAp t1 t2) = do
  k1 <- kindOfM t1
  k2 <- kindOfM t2
  applyKindM k1 k2
kindOfM (TFun t1 t2) = do
  k1 <- kindOfM t1
  k2 <- kindOfM t2
  case (k1, k2) of
    (Star, Star) -> return Star
    _ ->
      fail $
        "type malformed: function type of kinds from "
          ++ show k1
          ++ " to "
          ++ show k2

wellKindedM :: (MonadFail m) => Type i -> m (Type i)
wellKindedM t = kindOfM t >> pure t

validM :: (MonadFail m) => Type i -> m (Type i)
validM = wellKindedM

instantiateVarM :: (Eq i, MonadFail m) => (i, Type i) -> Type i -> m (Type i)
instantiateVarM subst t =
  let (si, st) = subst
   in case t of
        TVar k i -> do
          sk <- kindOfM st
          if sk == k
            then if si == i then return st else return t
            else
              fail $
                "instantiation of type variable failed: kinds mismatch "
                  ++ show k
                  ++ " +-> "
                  ++ show sk
        TAp t1 t2 -> do
          t1' <- instantiateVarM subst t1
          t2' <- instantiateVarM subst t2
          return $ TAp t1' t2'
        TFun t1 t2 -> do
          t1' <- instantiateVarM subst t1
          t2' <- instantiateVarM subst t2
          return $ TFun t1' t2'
        TConst _ _ -> pure t

eqTypeModNums :: Type i -> Type i -> Bool
eqTypeModNums t1 t2 = voidType t1 == voidType t2
