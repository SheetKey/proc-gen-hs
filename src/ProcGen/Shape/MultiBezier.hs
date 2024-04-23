{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE FunctionalDependencies #-}

module ProcGen.Shape.MultiBezier where

import ProcGen.Shape.Bezier

-- linear
import Linear

-- vector
import qualified Data.Vector as V

class (Eq a, Floating a, Epsilon a, Bezier (BezierType p) a) => MultiPoint p a | p -> a where
  type BezierType p
  toBezier :: (a -> a -> (a -> a)) -> p -> p -> BezierType p

-- | A 'CubicMP' is a "curve point with handles" for a cubic bezier.
-- Useful for generation.
data CubicMP a = CubicMP
  { bControl :: V3 a
  , bHandleLeft :: V3 a
  , bHandleRight :: V3 a
  , cphRadius :: a
  }
  deriving (Show)

instance (Eq a, Floating a, Epsilon a) => MultiPoint (CubicMP a) a where
  type BezierType (CubicMP a) = CubicBezier a
  toBezier mkTaper (CubicMP (V3 cx0 cy0 cz0) _ (V3 cx1 cy1 cz1) r1)
    (CubicMP (V3 cx3 cy3 cz3) (V3 cx2 cy2 cz2) _ r2)
    = CubicBezier { taperC = mkTaper r1 r2, ..}

newtype (Eq a, Floating a, Epsilon a, Bezier b a) => RawCurve b a = RawCurve (V.Vector b)

class (Eq a, Floating a, Epsilon a, MultiPoint p a, Bezier (BezierType p) a)
      => MultiBezier c p a | c -> p, p -> a where
  toRawCurve :: (a -> a -> (a -> a)) -> c -> RawCurve (BezierType p) a

newtype MultiPoint p a => Curve p a = Curve (V.Vector p)

instance {-# OVERLAPPABLE #-} MultiPoint (p a) a => MultiBezier (Curve (p a) a) (p a) a where
  toRawCurve mkTaper (Curve curve) =
    RawCurve $ V.zipWith (toBezier mkTaper) (V.init curve) (V.tail curve)
