{-# LANGUAGE TupleSections #-}
{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}

{-# HLINT ignore "Use const" #-}

module SnippyHs.Generation.ExprGenerator
  ( EG (..),
    EGContext (..),
    EGLocalContext (..),
    EGConstraints (..),
    generateExprEG,
  )
where

import Control.Monad (ap, liftM)
import Control.Monad.Crash (Crash (runCrash))
import Data.Data (toConstr)
import Data.List (nub)
import Data.Maybe (fromJust)
import Data.Monoid (All (..))
import Data.Result (Result (..))
import Data.Tuple (swap)
import Extra.Random (coinflip, shuffle)
import SnippyHs.Data.Term.Expr (Expr (..))
import SnippyHs.Data.Term.Kind (Kind (..))
import SnippyHs.Data.Term.Type
  ( Type (..),
    eqTypeModNums,
    fromTFunUnchecked,
    funCtr,
    kindOfM,
    literalTypes,
  )
import SnippyHs.Generation.GenerationScheme (ExprGenScheme (..))
import System.Random (StdGen, randomR)

type Type' = Type Int

type Expr' = Expr Int

type SymbolTable = [(Int, Type')]

type Result' = Result String

literalIds :: [(Int, Type')]
literalIds = zip [0 ..] literalTypes

data EGContext
  = EGontext
  { topLevelSymbolTable :: SymbolTable, -- symbol table
    typeVarsCount :: Int,
    namesCount :: Int,
    randomGenerator :: StdGen,
    availableSchemes :: [ExprGenScheme]
  }

data EGLocalContext
  = EGLocalContext
  { localTypeEnv :: [Type'],
    localSymbolTable :: SymbolTable
  }

isDerivableFrom :: EGContext -> EGLocalContext -> Type' -> ExprGenScheme -> Bool
isDerivableFrom ctx lctx t scheme =
  let k = runCrash (kindOfM t :: Crash Kind)
      env = localSymbolTable lctx ++ topLevelSymbolTable ctx
   in if k == Star
        then case scheme of
          ExprApp -> True
          SymbolInsertion ->
            any (\(_, st) -> eqTypeModNums t st) env
          LambdaAbstr -> toConstr t == funCtr
          LiteralInsertion -> t `elem` literalTypes
        else
          error $
            "isDerivableFrom failed: provided type "
              ++ show t
              ++ " has kind: "
              ++ show k

newtype EG a
  = EG {runEG :: EGContext -> (EGContext, a)}

instance Functor EG where
  fmap = liftM

instance Applicative EG where
  pure x = EG (,x)
  (<*>) = ap

instance Monad EG where
  return = pure
  EG f >>= g =
    EG
      ( \ctx0 ->
          let (ctx, x) = f ctx0
              EG gx = g x
           in gx ctx
      )

getContextEG :: EG EGContext
getContextEG = EG (\ctx -> (ctx, ctx))

putContextEG :: EGContext -> EG ()
putContextEG ctx = EG (\_ -> (ctx, ()))

getFieldEG :: (EGContext -> a) -> EG a
getFieldEG = (<$> getContextEG)

getTopLevelSymbolTableEG :: EG SymbolTable
getTopLevelSymbolTableEG =
  getFieldEG topLevelSymbolTable

getTypeVarsCountEG :: EG Int
getTypeVarsCountEG =
  getFieldEG typeVarsCount

getNamesCountEG :: EG Int
getNamesCountEG =
  getFieldEG namesCount

getRandomGeneratorEG :: EG StdGen
getRandomGeneratorEG =
  getFieldEG randomGenerator

getAvailableSchemesEG :: EG [ExprGenScheme]
getAvailableSchemesEG =
  getFieldEG availableSchemes

-- insertSymbol' :: (Int, Type') -> EGContext -> EGContext
-- insertSymbol' entry ctx =
--   ctx
--     { topLevelSymbolTable =
--         insertSymbol entry (topLevelSymbolTable ctx)
--     }

changeRandomGenerator' :: StdGen -> EGContext -> EGContext
changeRandomGenerator' rndGen ctx =
  ctx {randomGenerator = rndGen}

-- putSymbolEG :: (Int, Type') -> EG ()
-- putSymbolEG entry =
--   EG
--     ( \ctx ->
--         (insertSymbol' entry ctx, ())
--     )

putRandomGeneratorEG :: StdGen -> EG ()
putRandomGeneratorEG rndGen =
  EG (\ctx -> (changeRandomGenerator' rndGen ctx, ()))

randomActionEG :: (StdGen -> (a, StdGen)) -> EG a
randomActionEG act = do
  gen <- getRandomGeneratorEG
  let (r, gen') = act gen
  putRandomGeneratorEG gen'
  return r

randomEG :: (Int, Int) -> EG Int
randomEG range =
  randomActionEG $ randomR range

coinflipEG :: EG Bool
coinflipEG =
  randomActionEG coinflip

shuffleEG :: [a] -> EG [a]
shuffleEG xs =
  randomActionEG $ shuffle xs

typeEnvEG :: EGLocalContext -> EG [Type']
typeEnvEG lctx = do
  topSymtable <- getTopLevelSymbolTableEG
  let localSymtable = localSymbolTable lctx
  return $
    nub (map snd $ localSymtable ++ topSymtable)

randomExprAppArgTypeEG :: EGLocalContext -> EG (Type', EGLocalContext)
randomExprAppArgTypeEG lctx = do
  let lenv = localTypeEnv lctx
  tlenv <- typeEnvEG lctx
  cf <- coinflipEG
  if cf
    then do
      t <- newTypeVarEG
      let lenv' = t : lenv
      return (t, lctx {localTypeEnv = lenv'})
    else do
      let env = lenv ++ tlenv
      r <- randomEG (0, length env - 1)
      return (env !! r, lctx)

incrementTypeVarCnt :: EGContext -> EGContext
incrementTypeVarCnt ctx =
  ctx {typeVarsCount = typeVarsCount ctx + 1}

incrementNames :: EGContext -> EGContext
incrementNames ctx =
  ctx {namesCount = namesCount ctx + 1}

incrementTypeVarCntEG :: EG Int
incrementTypeVarCntEG = do
  ctx <- getContextEG
  putContextEG $ incrementTypeVarCnt ctx
  getTypeVarsCountEG

incrementNamesEG :: EG Int
incrementNamesEG = do
  ctx <- getContextEG
  putContextEG $ incrementNames ctx
  getNamesCountEG

newTypeVarEG :: EG Type'
newTypeVarEG = do
  TVar Star <$> incrementTypeVarCntEG

data EGConstraints = EGConstraints
  { targetType :: Type Int,
    targetDepth :: Int
  }

appropriateSchemesEG :: EGConstraints -> EGLocalContext -> EG [ExprGenScheme]
appropriateSchemesEG constrs lctx = do
  let EGConstraints tt tdepth = constrs
  let depthP :: ExprGenScheme -> Int -> Bool
      depthP ExprApp = (> 1)
      depthP LambdaAbstr = (> 1)
      depthP LiteralInsertion = (== 1)
      depthP SymbolInsertion = (== 1)
  ctx <- getContextEG
  let constrsP =
        [ (`depthP` tdepth),
          isDerivableFrom ctx lctx tt
        ]
  filter (\s -> getAll $ mconcat $ map (All . ($ s)) constrsP)
    <$> getAvailableSchemesEG

shuffledSchemesEG :: [ExprGenScheme] -> EG [ExprGenScheme]
shuffledSchemesEG schemes = do
  gen <- getRandomGeneratorEG
  let (shuffled, newGen) = shuffle schemes gen
  putRandomGeneratorEG newGen
  return shuffled

generateExprEG'' ::
  ExprGenScheme -> EGConstraints -> EGLocalContext -> EG (Result' Expr')
generateExprEG'' ExprApp constrs lctx0 = do
  let tdepth = targetDepth constrs
  let tt = targetType constrs
  (leftType, lctx) <- randomExprAppArgTypeEG lctx0
  resGenRight <-
    generateExprEG
      (constrs {targetType = leftType, targetDepth = tdepth - 1})
      lctx
  case resGenRight of
    Err _ -> return resGenRight
    Ok right -> do
      resGenLeft <-
        generateExprEG
          (constrs {targetType = TFun leftType tt, targetDepth = tdepth - 1})
          lctx
      return (resGenLeft >>= (\left -> pure $ App left right))
generateExprEG'' LambdaAbstr constrs lctx = do
  let tdepth = targetDepth constrs
  let tt = targetType constrs
  let (tleft, tright) = fromTFunUnchecked tt
  let localSymtable = localSymbolTable lctx
  nameInd <- incrementNamesEG
  resGenBody <-
    generateExprEG
      (constrs {targetType = tright, targetDepth = tdepth - 1})
      (lctx {localSymbolTable = (nameInd, tleft) : localSymtable})
  return $ case resGenBody of
    Err _ -> resGenBody
    Ok body -> Ok (ELambda [nameInd] body)
generateExprEG'' LiteralInsertion constrs _ = do
  let tt = targetType constrs
  return $
    Ok (EName $ fromJust $ lookup tt (map swap literalIds))
generateExprEG'' SymbolInsertion constrs lctx = do
  let tt = targetType constrs
  topSymtable <- getTopLevelSymbolTableEG
  shuffledEnv <- shuffleEG (localSymbolTable lctx ++ topSymtable)
  return $
    Ok (EName $ fromJust $ lookup tt (map swap shuffledEnv))

generateExprEG' ::
  [ExprGenScheme] -> EGConstraints -> EGLocalContext -> EG (Result' Expr')
generateExprEG' [] constrs _ =
  pure
    ( Err $
        "Generating expression of type "
          ++ show (targetType constrs)
          ++ " failed: ran out of schemes to try"
    )
generateExprEG' (scheme : schemeQueue) constrs lctx = do
  res <- generateExprEG'' scheme constrs lctx
  case res of
    Err _ -> generateExprEG' schemeQueue constrs lctx
    _ -> return res

generateExprEG ::
  EGConstraints -> EGLocalContext -> EG (Result' Expr')
generateExprEG constrs lctx = do
  schemes <- appropriateSchemesEG constrs lctx
  schemesQueue <- shuffledSchemesEG schemes
  generateExprEG' schemesQueue constrs lctx
