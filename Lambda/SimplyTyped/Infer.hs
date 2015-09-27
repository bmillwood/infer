module Lambda.SimplyTyped.Infer where

import qualified Data.Map as M

import Control.Monad.Reader
import Control.Monad.State
import Control.Monad.Writer
import Data.Void

import Lambda.Untyped
import Lambda.SimplyTyped

data Equation t = Type t :=: Type t
  deriving (Eq, Ord, Read, Show)

onETypes :: (Type t -> Type t) -> Equation t -> Equation t
onETypes f (t1 :=: t2) = f t1 :=: f t2

data Supply n = n :> Supply n
  deriving (Eq, Ord, Read, Show)

positiveIntegers :: Supply Integer
positiveIntegers = from 0
  where
    from n = n :> from (n + 1)

data InferError
  = OccursCheck

type InferMonad n t =
  ReaderT (M.Map n (Type t))
    (WriterT [Equation t]
      (StateT (Supply t)
        (Either InferError)))

newTyVar :: InferMonad n t t
newTyVar = state (\ (n :> ns) -> (n, ns))

generateEquations :: (Ord n) => Exp Void n -> InferMonad n Integer (TExp Void Integer n)
generateEquations (Lit void) = absurd void
generateEquations (Var name) = do
  env <- ask
  case M.lookup name env of
    Nothing -> do
      n <- newTyVar
      return (TVar (Base n) name)
    Just t -> return (TVar t name)
generateEquations (App e1 e2) = do
  t1 <- generateEquations e1
  t2 <- generateEquations e2
  an <- newTyVar
  rn <- newTyVar
  tell [getType t1 :=: Arrow (Base an) (Base rn), getType t2 :=: Base an]
  return (TApp (Base rn) t1 t2)
generateEquations (Lam n e) = do
  vn <- newTyVar
  te <- local (M.insert n (Base vn)) (generateEquations e)
  return (TLam (Arrow (Base vn) (getType te)) n (Base vn) te)        

infer :: (Ord n) => Exp Void n -> Either InferError (TExp Void Integer n)
infer e = do
    ((exp, equations), _finalState) <-
      runStateT (runWriterT (runReaderT (generateEquations e) M.empty)) positiveIntegers
    applyEquations equations exp
  where
    subst :: (Eq t) => t -> Type t -> Type t -> Type t
    subst u twith tin = tin >>= k
      where
        k v | u == v = twith | otherwise = Base v
    applyEquations [] e = Right e
    applyEquations (Base t :=: t' : eqns) e
      | occurs t t' = Left OccursCheck
      | otherwise = do
          let k = subst t t'
          applyEquations (map (onETypes k) eqns) (onTypes k e)
    applyEquations (t' :=: Base t : eqns) e =
      applyEquations (Base t :=: t' : eqns) e
    applyEquations (Arrow t1 t2 :=: Arrow t3 t4 : eqns) e =
      applyEquations (t1 :=: t3 : t2 :=: t4 : eqns) e
    occurs u (Base v) = u == v
    occurs u (Arrow t1 t2) = occurs u t1 || occurs u t2
