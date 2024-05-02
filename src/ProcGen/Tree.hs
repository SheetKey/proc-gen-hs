{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE RecordWildCards #-}

module ProcGen.Tree where

import MathUtil
import ProcGen.Shape.MultiBezier
import ProcGen.Turtle

-- base
import Control.Arrow (first)

-- linear
import Linear

-- vector
import qualified Data.Vector as V

-- MonadRandom
import Control.Monad.Random

-- mtl
import Control.Monad.Reader.Class
import Control.Monad.State.Class

-- containers
import qualified Data.IntMap.Lazy as M

data BranchMode = Fan | Whorled | AltOpp

data PShape = Spherical
            | Hemispherical
            | Cylindrical
            | TaperedCylindrical
            | Flame
            | InverseConical
            | TendFlame
            | Envelope
            | Conical

data Parameters = Parameters
  { pShape          :: PShape    
  , pGScale         :: Double    
  , pGScaleV        :: Double    
  , pLevels         :: Int
  , pMaxLevel       :: Int
  , pRatio          :: Double
  , pRatioPower     :: Double
  , pFlare          :: Double
  , pBaseSplits     :: Int
  , pBaseSize       :: V.Vector Double
  , pDownAngle      :: V.Vector Double
  , pDownAngleV     :: V.Vector Double
  , pRotate         :: V.Vector Double
  , pRotateV        :: V.Vector Double
  , pBranches       :: V.Vector Int   
  , pLength         :: V.Vector Double
  , pLengthV        :: V.Vector Double
  , pTaper          :: V.Vector Double
  , pSegSplits      :: V.Vector Double
  , pSplitAngle     :: V.Vector Double
  , pSplitAngleV    :: V.Vector Double
  , pCurveRes       :: V.Vector Int   
  , pCurve          :: V.Vector Double
  , pCurveBack      :: V.Vector Double
  , pCurveV         :: V.Vector Double
  , pBendV          :: V.Vector Double
  , pBranchDist     :: V.Vector Double
  , pRadiusMod      :: V.Vector Double
  , pTropism        :: V3 Double
  , pPruneRatio     :: Double   
  , pPruneWidth     :: Double   
  , pPruneWidthPeak :: Double   
  , pPrunePowerLow  :: Double   
  , pPrunePowerHigh :: Double   
  }

validateParameters :: Parameters -> Either String Parameters
validateParameters Parameters {..}
  | pGScale <= 0 = Left "'pGScale' must be greater than 0."
  | pLevels <= 0 = Left "'pLevels' must be greater than 0."
  | pMaxLevel <= 0 = Left "'pMaxLevel' must be greater than 0."
  | pRatio <= 0 = Left "'pRation' must be greater than 0."
  | V.length pBaseSize    /= (pMaxLevel + 1) = Left "'pBaseSize' must have length 'pMaxLevel + 1'"
  | V.length pDownAngle   /= (pMaxLevel + 1) = Left "'pDownAngle' must have length 'pMaxLevel + 1'"
  | V.length pDownAngleV  /= (pMaxLevel + 1) = Left "'pDownAngleV' must have length 'pMaxLevel + 1'"
  | V.length pRotate      /= (pMaxLevel + 1) = Left "'pRotate' must have length 'pMaxLevel + 1'"
  | V.length pRotateV     /= (pMaxLevel + 1) = Left "'pRotateV' must have length 'pMaxLevel + 1'"
  | V.length pBranches    /= (pMaxLevel + 1) = Left "'pBranches' must have length 'pMaxLevel + 1'"
  | V.length pLength      /= (pMaxLevel + 1) = Left "'pLength' must have length 'pMaxLevel + 1'"
  | V.length pLengthV     /= (pMaxLevel + 1) = Left "'pLengthV' must have length 'pMaxLevel + 1'"
  | V.length pTaper       /= (pMaxLevel + 1) = Left "'pTaper' must have length 'pMaxLevel + 1'"
  | V.length pSegSplits   /= (pMaxLevel + 1) = Left "'pSegSplits' must have length 'pMaxLevel + 1'"
  | V.length pSplitAngle  /= (pMaxLevel + 1) = Left "'pSplitAngle' must have length 'pMaxLevel + 1'"
  | V.length pSplitAngleV /= (pMaxLevel + 1) = Left "'pSplitAngleV' must have length 'pMaxLevel + 1'"
  | V.length pCurveRes    /= (pMaxLevel + 1) = Left "'pCurveRes' must have length 'pMaxLevel + 1'"
  | V.length pCurve       /= (pMaxLevel + 1) = Left "'pCurve' must have length 'pMaxLevel + 1'"
  | V.length pCurveBack   /= (pMaxLevel + 1) = Left "'pCurveBack' must have length 'pMaxLevel + 1'"
  | V.length pCurveV      /= (pMaxLevel + 1) = Left "'pCurveV' must have length 'pMaxLevel + 1'"
  | V.length pBendV       /= (pMaxLevel + 1) = Left "'pBendV' must have length 'pMaxLevel + 1'"
  | V.length pBranchDist  /= (pMaxLevel + 1) = Left "'pBranchDist' must have length 'pMaxLevel + 1'"
  | V.length pRadiusMod   /= (pMaxLevel + 1) = Left "'pRadiusMod' must have length 'pMaxLevel + 1'"
  | pBaseSplits < 0 = Left "'pBaseSplits' must be greater than or equal to 0."
  | V.any (<= 0) pLength = Left "'pLength' must be greater than 0."
  | V.any (< 0) pTaper = Left "'pTaper' must be greater than or equal to 0."
  | V.any (> 3) pTaper = Left "'pTaper' must be less than or equal to 3."
  | V.any (< 0) pSegSplits = Left "'pSegSplits' must be greater than or equal to 0."
  | V.any (> 2) pSegSplits = Left "'pSegSplits' must be less than or equal to 2."
  | V.any (<= 0) pCurveRes = Left "'pCurveRes' must be greater than 0."
  | V.any (< 0) pBranchDist = Left "'pBranchDist' must be greater than or equal to 0."
  | V.any (< 0) pRadiusMod = Left "'pRadiusMod' must be greater than or equal to 0."
  | pPruneRatio < 0 = Left "'pPruneRatio' must be greater than or equal to 0."
  | pPruneRatio > 1 = Left "'pPruneRatio' must be less than or equal to 1."
  | pPruneWidth <= 0 = Left "'pPruneWidth' must be greater than 0."
  | pPruneWidthPeak < 0 = Left "'pPruneWidthPeak' must be greater than or equal to 0."
  | otherwise = Right $ Parameters {..}

data Stem = Stem
  { sDepth :: Int
  , sCurve :: CubicCurve Double
  , sParent :: Maybe Int
  , sOffset :: Double
  , sRadiusLimit :: Double
  , sLength :: Double
  , sRadius :: Double
  , sLengthChildMax :: Double
  , sIndex :: Int
  }

data Tree = Tree
  { tTreeScale :: Double
  , tBaseLength :: Double
  , tSplitNumError :: V.Vector Double
  , tTrunkLength :: Double
  , tStems :: V.Vector Stem
  }

-- | Create a "blank" stem
stemFromDepth :: Int -> Stem
stemFromDepth sDepth = Stem
  { sCurve          = mempty
  , sParent         = Nothing
  , sOffset         = 0
  , sRadiusLimit    = -1
  , sLength         = 0
  , sRadius         = 0
  , sLengthChildMax = 0
  , sIndex          = -1
  , ..
  }

data TBState = TBState
  { tree :: Tree
  , stems :: M.IntMap Stem
  , stemKeys :: ([Int], Int)
  , turtles :: M.IntMap Turtle
  , turtleKeys :: ([Int], Int)
  }

newtype TreeBuilder g a = TB
  { runTB :: StemKey -> TurtleKey -> Parameters -> g -> TBState -> (a, TBState, g) }

instance Functor (TreeBuilder g) where
  fmap f tb = TB $ \ sk tk p g s ->
    let ~(a, s', g') = runTB tb sk tk p g s
    in (f a, s', g')
  {-# INLINE fmap #-}

instance Applicative (TreeBuilder g) where
  pure a = TB $ \ _ _ _ g s -> (a, s, g)
  {-# INLINE pure #-}
  tbf <*> tba = TB $ \ sk tk p g s ->
    let ~(f, s', g') = runTB tbf sk tk p g s
        ~(v, s'', g'') = runTB tba sk tk p g' s'
    in (f v, s'', g'')
  {-# INLINE (<*>) #-}

instance Monad (TreeBuilder g) where
  tba >>= tbf = TB $ \ sk tk p g s ->
    let ~(a, s', g') = runTB tba sk tk p g s
        ~(b, s'', g'') = runTB (tbf a) sk tk p g' s'
    in (b, s'', g'')
  {-# INLINE (>>=) #-}

instance RandomGen g => MonadRandom (TreeBuilder g) where
  getRandomR lohi = TB $ \ _ _ _ g s -> let ~(a, g') = randomR lohi g in (a, s, g')
  getRandom = TB $ \ _ _ _ g s -> let ~(a, g') = random g in (a, s, g')
  getRandomRs lohi = TB $ \ _ _ _ g s ->
                            let ~(as, g') = (first (randomRs lohi) . split) g
                            in (as, s, g')
  getRandoms = TB $ \ _ _ _ g s ->
                      let ~(as, g') = (first randoms . split) g
                      in (as, s, g')

getRandomState :: TreeBuilder g g
getRandomState = TB $ \ _ _ _ g s -> (g, s, g)

setRandomState :: g -> TreeBuilder g ()
setRandomState g = TB $ \ _ _ _ _ s -> ((), s, g)

instance MonadReader Parameters (TreeBuilder g) where
  ask = TB $ \ _ _ p g s -> (p, s, g)
  {-# INLINE ask #-}
  reader f = TB $ \ _ _ p g s -> (f p, s, g)
  {-# INLINE reader #-}
  local f tb = TB $ \ sk tk p g s -> runTB tb sk tk (f p) g s
  {-# INLINE local #-}

instance MonadState TBState (TreeBuilder g) where
  get = TB $ \ _ _ _ g s -> (s, s, g)
  {-# INLINE get #-}
  put s = TB $ \ _ _ _ g _ -> ((), s, g)
  {-# INLINE put #-}
  state f = TB $ \ _ _ _ g s -> let ~(a, s') = f s in (a, s', g)
  {-# INLINE state #-}

putStem :: Stem -> TreeBuilder g StemKey
putStem stem = state $ \ TBState {..} ->
  case stemKeys of
    ([], key) ->
      ( key
      , TBState
        { stems = M.insert key stem stems, stemKeys = ([], key + 1), .. }
      )
    (key:keys, nextKey) ->
      ( key
      , TBState
        { stems = M.insert key stem stems, stemKeys = (keys, nextKey), .. }
      )

putTurtle :: Turtle -> TreeBuilder g TurtleKey
putTurtle turtle = state $ \ TBState {..} ->
  case turtleKeys of
    ([], key) ->
      ( key
      , TBState
        { turtles = M.insert key turtle turtles, turtleKeys = ([], key + 1), .. }
      )
    (key:keys, nextKey) ->
      ( key
      , TBState
        { turtles = M.insert key turtle turtles, turtleKeys = (keys, nextKey), .. }
      )
      
type StemKey = Int
type TurtleKey = Int

modifyStemKey :: StemKey -> (Stem -> Stem) -> TreeBuilder g ()
modifyStemKey key f = modify $ \ TBState {..} -> TBState
  { stems = M.adjust f key stems, .. }

modifyTurtleKey :: TurtleKey -> (Turtle -> Turtle) -> TreeBuilder g ()
modifyTurtleKey key f = modify $ \ TBState {..} -> TBState
  { turtles = M.adjust f key turtles, .. }

modifyStem :: (Stem -> Stem) -> TreeBuilder g ()
modifyStem f = TB $ \ sk _ _ g TBState {..} ->
  ((), TBState { stems = M.adjust f sk stems, ..}, g)

modifyTurtle :: (Turtle -> Turtle) -> TreeBuilder g ()
modifyTurtle f = TB $ \ _ tk _ g TBState {..} ->
  ((), TBState { turtles = M.adjust f tk turtles, .. }, g)

modifyTree :: (Tree -> Tree) -> TreeBuilder g ()
modifyTree f = modify $ \ TBState {..} -> TBState
  { tree = f tree, .. }

getStemKey :: StemKey -> TreeBuilder g Stem
getStemKey key = (M.! key) . stems <$> get

getTurtleKey :: TurtleKey -> TreeBuilder g Turtle
getTurtleKey key = (M.! key) . turtles <$> get

getStem :: TreeBuilder g Stem
getStem = TB $ \ sk _ _ g s@(TBState {..}) -> (stems M.! sk, s, g)

getTurtle :: TreeBuilder g Turtle
getTurtle = TB $ \ _ tk _ g s@(TBState {..}) -> (turtles M.! tk, s, g)

getTree :: TreeBuilder g Tree
getTree = tree <$> get

deleteStemKey :: StemKey -> TreeBuilder g ()
deleteStemKey key = modify $ \ TBState {..} ->
  case stemKeys of
    (keys, nextKey) -> TBState
      { stems = M.delete key stems, stemKeys = (key:keys, nextKey), .. }

deleteTurtleKey :: TurtleKey -> TreeBuilder g ()
deleteTurtleKey key = modify $ \ TBState {..} ->
  case turtleKeys of
    (keys, nextKey) -> TBState
      { turtles = M.delete key turtles, turtleKeys = (key:keys, nextKey), .. }

adjustStemKey :: StemKey -> Stem -> TreeBuilder g ()
adjustStemKey key stem = modify $ \ TBState {..} -> TBState
  { stems = M.insert key stem stems, .. }

adjustTurtleKey :: TurtleKey -> Turtle -> TreeBuilder g ()
adjustTurtleKey key turtle = modify $ \ TBState {..} -> TBState
  { turtles = M.insert key turtle turtles, .. }

adjustStem :: Stem -> TreeBuilder g ()
adjustStem stem = TB $ \ sk _ _ g TBState {..} ->
  ((), TBState { stems = M.insert sk stem stems, .. }, g)

adjustTurtle :: Turtle -> TreeBuilder g ()
adjustTurtle turtle = TB $ \ _ tk _ g TBState {..} ->
  ((), TBState { turtles = M.insert tk turtle turtles, .. }, g)

adjustTree :: Tree -> TreeBuilder g ()
adjustTree tree' = modify $ \ TBState {..} -> TBState { tree = tree', ..}

stateStemKey :: StemKey -> (Stem -> (a, Stem)) -> TreeBuilder g a
stateStemKey key f = do
  stem <- getStemKey key
  let (a, stem') = f stem
  adjustStemKey key stem'
  return a

stateTurtleKey :: TurtleKey -> (Turtle -> (a, Turtle)) -> TreeBuilder g a
stateTurtleKey key f = do
  turtle <- getTurtleKey key
  let (a, turtle') = f turtle
  adjustTurtleKey key turtle'
  return a

stateStem :: (Stem -> (a, Stem)) -> TreeBuilder g a
stateStem f = do
  stem <- getStem
  let (a, stem') = f stem
  adjustStem stem'
  return a

stateTurtle :: (Turtle -> (a, Turtle)) -> TreeBuilder g a
stateTurtle f = do
  turtle <- getTurtle
  let (a, turtle') = f turtle
  adjustTurtle turtle'
  return a

stateTree :: (Tree -> (a, Tree)) -> TreeBuilder g a
stateTree f = do
  tree <- getTree
  let (a, tree') = f tree
  adjustTree tree'
  return a

useStemKey :: StemKey -> (Stem -> a) -> TreeBuilder g a
useStemKey key f = do
  stem <- getStemKey key
  return $ f stem

useTurtleKey :: TurtleKey -> (Turtle -> a) -> TreeBuilder g a
useTurtleKey key f = do
  turtle <- getTurtleKey key
  return $ f turtle

useStem :: (Stem -> a) -> TreeBuilder g a
useStem = (<$> getStem)

useTurtle :: (Turtle -> a) -> TreeBuilder g a
useTurtle = (<$> getTurtle)

useTree :: (Tree -> a) -> TreeBuilder g a
useTree f = f <$> getTree

useStemKeyM :: StemKey -> (Stem -> TreeBuilder g a) -> TreeBuilder g a
useStemKeyM key f = do
  stem <- getStemKey key
  f stem

useTurtleKeyM :: TurtleKey -> (Turtle -> TreeBuilder g a) -> TreeBuilder g a
useTurtleKeyM key f = do
  turtle <- getTurtleKey key
  f turtle

useStemM :: (Stem -> TreeBuilder g a) -> TreeBuilder g a
useStemM f = getStem >>= f

useTurtleM :: (Turtle -> TreeBuilder g a) -> TreeBuilder g a
useTurtleM f = getTurtle >>= f

useTreeM :: (Tree -> TreeBuilder g a) -> TreeBuilder g a
useTreeM f = getTree >>= f

useParentKey :: StemKey -> (Stem -> a) -> TreeBuilder g a
useParentKey key f = useStemKeyM key $ \ Stem { sParent } ->
  case sParent of
    Nothing -> error "stem has no parent"
    Just pkey -> useStemKey pkey f

useParent :: (Stem -> a) -> TreeBuilder g a
useParent f = useStemM $ \ Stem { sParent } ->
  case sParent of
    Nothing -> error "stem has no parent"
    Just pkey -> useStemKey pkey f

useParentKeyM :: StemKey -> (Stem -> TreeBuilder g a) -> TreeBuilder g a
useParentKeyM key f = useStemKeyM key $ \ Stem { sParent } ->
  case sParent of
    Nothing -> error "stem has no parent"
    Just pkey -> useStemKeyM pkey f

useParentM :: (Stem -> TreeBuilder g a) -> TreeBuilder g a
useParentM f = useStemM $ \ Stem { sParent } ->
  case sParent of
    Nothing -> error "stem has no parent"
    Just pkey -> useStemKeyM pkey f
    
calcHelixPoints :: RandomGen g => Double -> Double
                -> TreeBuilder g (V3 Double, V3 Double, V3 Double, V3 Double)
calcHelixPoints rad pitch = useTurtleM $ \Turtle {..} -> do
  spinAng <- getRandomR (0, 2 * pi)
  let p0 = V3 0 (negate rad) (negate pitch / 4)
      p1 = V3 (4 * rad/ 3) (negate rad) 0
      p2 = V3 (4 * rad / 3) rad 0
      p3 = V3 0 rad (pitch / 4) 
      trf = toTrackQuatZY turtleDir
      rotQuat = axisAngle (V3 0 0 1) spinAng
      p0' = rotate trf $ rotate rotQuat p0
      p1' = rotate trf $ rotate rotQuat p1
      p2' = rotate trf $ rotate rotQuat p2
      p3' = rotate trf $ rotate rotQuat p3
  return (p1' - p0', p2' - p0', p3' - p0', turtleDir)

calcShapeRatio :: PShape -> Double -> TreeBuilder g Double
calcShapeRatio shape ratio =
  case shape of
    Spherical -> return $ 0.2 + 0.8 * sin (pi * ratio)
    Hemispherical -> return $ 0.2 + 0.8 * sin (0.5 * pi * ratio)
    Cylindrical -> return 1
    TaperedCylindrical -> return $ 0.5 + 0.5 * ratio
    Flame -> return $ if ratio <= 0.7 then ratio / 0.7 else (1 - ratio) / 0.3
    InverseConical -> return $ 1 - 0.8 * ratio
    TendFlame -> return $ if ratio <= 0.7
                          then 0.5 + 0.5 * ratio / 0.7
                          else 0.5 + 0.5 * (1 - ratio) / 0.3
    Envelope -> if ratio < 0 || ratio > 1
         then return 0
         else do Parameters {..} <- ask
                 if ratio < 1 - pPruneWidthPeak
                   then return $ (ratio / (1 - pPruneWidthPeak)) ** pPrunePowerHigh
                   else return $ ((1 - ratio) / (1 - pPruneWidthPeak)) ** pPrunePowerLow
    Conical -> return $ 0.2 + 0.8 * ratio

calcStemLength :: RandomGen g => TreeBuilder g Double
calcStemLength = useStemM $ \ Stem {..} -> do
  Parameters {..} <- ask
  result <- case sDepth of
              -- trunk
              0 -> do 
                r <- getRandomR (-1, 1)
                stateTree $ \ Tree {..} ->
                  let tTrunkLength' = tTreeScale * ((pLength V.! 0) + r * (pLengthV V.! 0))
                  in (tTrunkLength', Tree { tTrunkLength = tTrunkLength', .. })
              -- first level
              1 -> useParentM $
                \ Stem { sLength = psLength, sLengthChildMax = psLengthChildMax } ->
                  useTreeM $
                  \ Tree {..} -> do
                    let ratio = (psLength - sOffset) / (psLength - tBaseLength)
                    shapeRatio <- calcShapeRatio pShape ratio
                    return $ psLength * psLengthChildMax * shapeRatio
              _ -> useParent $
                \ Stem { sLength = psLength, sLengthChildMax = psLengthChildMax } ->
                  psLengthChildMax * (psLength - 0.7 * sOffset)
  return $ max 0 result

calcStemRadius :: TreeBuilder g Double
calcStemRadius = useStemM $ \ Stem {..} -> do
  Parameters {..} <- ask
  if sDepth == 0
    then return $ sLength * (pRatio * (pRadiusMod V.! 0))
    else useParent $ \ Stem { sRadius = psRadius, sLength = psLength } ->
                       min sRadiusLimit $ max 0.005 $
                       (pRadiusMod V.! sDepth) * psRadius * ((sLength / psLength) ** pRatioPower)

calcCurveAngle :: RandomGen g => Int -> Int -> TreeBuilder g Double
calcCurveAngle depth segInd = do
  Parameters {..} <- ask
  let curve = pCurve V.! depth
      curveBack = pCurveBack V.! depth
      curveRes = fromIntegral $ pCurveRes V.! depth
      curveV = pCurveV V.! depth
      curveAngle | curveBack == 0 = curve / curveRes
                 | fromIntegral segInd < curveRes / 2 = curve / (curveRes / 2)
                 | otherwise = curveBack / (curveRes / 2)
  r <- getRandomR (-1, 1)
  return $ curveAngle + (r * (curveV / curveRes))

calcDownAngle :: RandomGen g => Double -> TreeBuilder g Double
calcDownAngle stemOffset = useStemM $ \ Stem {..} -> do
  Parameters {..} <- ask
  let dp1 = min (sDepth + 1) pMaxLevel
      downAngleV = pDownAngleV V.! dp1
  if downAngleV >= 0
    then do r <- getRandomR (-1, 1)
            return $ (pDownAngle V.! dp1) + (r * downAngleV)
    else do shapeRatio <- calcShapeRatio Spherical $
                          (sLength - stemOffset) / (sLength * (1 - pBaseSize V.! sDepth))
            let downAngle = downAngleV + (downAngleV * (1 - 2 * shapeRatio))
            r <- getRandomR (-1, 1)
            return $ downAngle + (r * abs (downAngle * 0.1))
