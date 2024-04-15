{-# LANGUAGE RecordWildCards #-}

module ProcGen.Tree where

-- vector
import qualified Data.Vector as V 

data BranchMode = Fan | Whorled | AltOpp

data PShape = Spherical          -- 0
            | Hemispherical      -- 1
            | Cylindrical        -- 2
            | TaperedCylindrical -- 3
            | Flame              -- 4
            | InverseConical     -- 5
            | TendFlame          -- 6
            | Envelope           -- 7
            | Conical            -- 8

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
  , pLeafBlosNum    :: Int   
  , pLeafShape      :: Int   
  , pLeafScale      :: Double
  , pLeafScaleX     :: Double
  , pLeafBend       :: Double
  , pBlossomShape   :: Int   
  , pBlossomScale   :: Double
  , pBlossomRate    :: Double
  , pTropism        :: V3 Double
  , pPruneRatio     :: Double   
  , pPruneWidth     :: Double   
  , pPruneWidthPeak :: Double   
  , pPrunePowerLow  :: Double   
  , pPrunePowerHigh :: Double   
  }

validateParameters :: Parameters -> Either String Parameters
validateParameters Parameters {..}
  | pGScale <= 0 = Left $ "'pGScale' must be greater than 0."
  | pLevels <= 0 = Left $ "'pLevels' must be greater than 0."
  | pMaxLevel <= 0 = Left $ "'pMaxLevel' must be greater than 0."
  | pRatio <= 0 = Left $ "'pRation' must be greater than 0."
  | V.length pBaseSplits  /= (pMaxLevel + 1) = Left $ "'pBaseSplits' must have length 'pMaxLevel + 1'"
  | V.length pBaseSize    /= (pMaxLevel + 1) = Left $ "'pBaseSize' must have length 'pMaxLevel + 1'"
  | V.length pDownAngle   /= (pMaxLevel + 1) = Left $ "'pDownAngle' must have length 'pMaxLevel + 1'"
  | V.length pDownAngleV  /= (pMaxLevel + 1) = Left $ "'pDownAngleV' must have length 'pMaxLevel + 1'"
  | V.length pRotate      /= (pMaxLevel + 1) = Left $ "'pRotate' must have length 'pMaxLevel + 1'"
  | V.length pRotateV     /= (pMaxLevel + 1) = Left $ "'pRotateV' must have length 'pMaxLevel + 1'"
  | V.length pBranches    /= (pMaxLevel + 1) = Left $ "'pBranches' must have length 'pMaxLevel + 1'"
  | V.length pLength      /= (pMaxLevel + 1) = Left $ "'pLength' must have length 'pMaxLevel + 1'"
  | V.length pLengthV     /= (pMaxLevel + 1) = Left $ "'pLengthV' must have length 'pMaxLevel + 1'"
  | V.length pTaper       /= (pMaxLevel + 1) = Left $ "'pTaper' must have length 'pMaxLevel + 1'"
  | V.length pSegSplits   /= (pMaxLevel + 1) = Left $ "'pSegSplits' must have length 'pMaxLevel + 1'"
  | V.length pSplitAngle  /= (pMaxLevel + 1) = Left $ "'pSplitAngle' must have length 'pMaxLevel + 1'"
  | V.length pSplitAngleV /= (pMaxLevel + 1) = Left $ "'pSplitAngleV' must have length 'pMaxLevel + 1'"
  | V.length pCurveRes    /= (pMaxLevel + 1) = Left $ "'pCurveRes' must have length 'pMaxLevel + 1'"
  | V.length pCurve       /= (pMaxLevel + 1) = Left $ "'pCurve' must have length 'pMaxLevel + 1'"
  | V.length pCurveBack   /= (pMaxLevel + 1) = Left $ "'pCurveBack' must have length 'pMaxLevel + 1'"
  | V.length pCurveV      /= (pMaxLevel + 1) = Left $ "'pCurveV' must have length 'pMaxLevel + 1'"
  | V.length pBendV       /= (pMaxLevel + 1) = Left $ "'pBendV' must have length 'pMaxLevel + 1'"
  | V.length pBranchDist  /= (pMaxLevel + 1) = Left $ "'pBranchDist' must have length 'pMaxLevel + 1'"
  | V.length pRadiusMod   /= (pMaxLevel + 1) = Left $ "'pRadiusMod' must have length 'pMaxLevel + 1'"
  | V.any (< 0) pBaseSplits = Left $ "'pBaseSplits' must be greater than or equal to 0."
  | V.any (<= 0) pLength = Left $ "'pLength' must be greater than 0."
  | V.any (< 0) pTaper = Left $ "'pTaper' must be greater than or equal to 0."
  | V.any (> 3) pTaper = Left $ "'pTaper' must be less than or equal to 3."
  | V.any (< 0) pSegSplits = Left $ "'pSegSplits' must be greater than or equal to 0."
  | V.any (> 2) pSegSplits = Left $ "'pSegSplits' must be less than or equal to 2."
  | V.any (<= 0) pCurveRes = Left $ "'pCurveRes' must be greater than 0."
  | V.any (< 0) pBranchDist = Left $ "'pBranchDist' must be greater than or equal to 0."
  | V.any (< 0) pRadiusMod = Left $ "'pRadiusMod' must be greater than or equal to 0."
  | V.any (< 0) pLeafBlosNum = Left $ "'pLeafBlosNum' must be greater than or equal to 0."
  | V.any (<= 0) pLeafScaleX = Left $ "'pLeafScaleX' must be greater than 0."
  | V.any (< 0) pLeafBend = Left $ "'pLeafBend' must be greater than or equal to 0."
  | V.any (> 1) pLeafBend = Left $ "'pLeafBlend' must be less than or equal to 1."
  | V.any (<= 0) pBlossomScale = Left $ "'pBlossomScale' must be greater than 0."
  | V.any (< 0) pBlossomRate = Left $ "'pBlossomRate' must be greater than or equal to 0."
  | V.any (> 1) pBlossomRate = Left $ "'pBlossomRate' must be less than or equal to 1."
  | V.any (< 0) pPruneRatio = Left $ "'pPruneRatio' must be greater than or equal to 0."
  | V.any (> 1) pPruneRatio = Left $ "'pPruneRatio' must be less than or equal to 1."
  | V.any (<= 0) pPruneWidth = Left $ "'pPruneWidth' must be greater than 0."
  | V.any (< 0) pPruneWidthPeak = Left $ "'pPruneWidthPeak' must be greater than or equal to 0."
  | otherwise = Right $ Parameters {..}
