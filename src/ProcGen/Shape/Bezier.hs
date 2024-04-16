{-# LANGUAGE RecordWildCards #-}

module ProcGen.Shape.Bezier where

-- linear
import Linear

import qualified Data.Vector as V

-- | A 'CPH' is a "curve point with handles" for a cubic bezier.
-- Useful for generation.
data CPH a = CPH
  { bControl :: V3 a
  , bHandleLeft :: V3 a
  , bHandleRight :: V3 a
  , cphRadius :: a
  }
  deriving (Show)

-- | A 'Curve' is an ordered collection of 'CPH'. I.e., any two consecutive elements 
-- gives rise to a 'CubicBezier'.
type Curve a = V.Vector (CPH a)

-- | The raw form of a cubic bezier curve. Useful for math.
data Bezier a = Bezier
  { cx0 :: a
  , cy0 :: a
  , cz0 :: a
  , cx1 :: a
  , cy1 :: a
  , cz1 :: a
  , cx2 :: a
  , cy2 :: a
  , cz2 :: a
  , cx3 :: a
  , cy3 :: a
  , cz3 :: a
  , maxRadius :: a
  , taper :: a -> a
  }

-- A collection of bezier curves
type RawCurve a = V.Vector (Bezier a)

-- | A quadratic bezier is the derivative of a cubic bezier.
data QuadBezier a = QuadBezier
  { qx0 :: a
  , qy0 :: a
  , qz0 :: a
  , qx1 :: a
  , qy1 :: a
  , qz1 :: a
  , qx2 :: a
  , qy2 :: a
  , qz2 :: a
  }

fromCPH :: Ord a => (a -> a -> (a -> a)) -> CPH a -> CPH a -> Bezier a
fromCPH f (CPH (V3 cx0 cy0 cz0) _ (V3 cx1 cy1 cz1) r1)
  (CPH (V3 cx3 cy3 cz3) (V3 cx2 cy2 cz2) _ r2)
  = Bezier
    { maxRadius = max r1 r2
    , taper = f r1 r2
    , ..
    }

fromCurve :: Ord a => (a -> a -> (a -> a)) -> Curve a -> RawCurve a
fromCurve f curve = V.zipWith (fromCPH f) (V.init curve) (V.tail curve)

-- | Evaluate a 'CubicBezier' at a time.
-- Gives the point and the radius.
eval :: (Eq a, Fractional a) => Bezier a -> a -> (V3 a, a)
eval Bezier {..} time =
  case time of
    0 -> (V3 cx0 cy0 cz0, taper 0)
    1 -> (V3 cx3 cy3 cz3, taper 1)
    t -> let mt = 1 - t
             mt2 = mt * mt
             t2 = t * t
             a = mt2 * mt
             b = mt2 * t * 3
             c = mt * t2 * 3
             d = t * t2
             x = (a * cx0) + (b * cx1)
               + (c * cx2) + (d * cx3)
             y = (a * cy0) + (b * cy1)
               + (c * cy2) + (d * cy3)
             z = (a * cz0) + (b * cz1)
               + (c * cz2) + (d * cz3)
         in (V3 x y z, taper t)

evalCPH :: (Ord a, Fractional a) => CPH a -> CPH a -> a -> (V3 a, a)
evalCPH c1 c2 = eval (fromCPH (\ _ _ -> id) c1 c2)

-- | Calculate the derivative of a bezier curve
deriv :: Num a => Bezier a -> QuadBezier a
deriv Bezier {..} =
  let qx0 = 3 * (cx1 - cx0)
      qy0 = 3 * (cy1 - cy0)
      qz0 = 3 * (cz1 - cz0)
      qx1 = 3 * (cx2 - cx1)
      qy1 = 3 * (cy2 - cy1)
      qz1 = 3 * (cz2 - cz1)
      qx2 = 3 * (cx3 - cx2)
      qy2 = 3 * (cy3 - cy2)
      qz2 = 3 * (cz3 - cz2)
  in QuadBezier {..}

-- | Evaluate a quadratic bezier at a time.
evalQuad :: (Eq a, Fractional a) => QuadBezier a -> a -> V3 a
evalQuad QuadBezier {..} time =
  case time of
    0 -> (V3 qx0 qy0 qz0)
    1 -> (V3 qx2 qy2 qz2)
    t -> let mt = 1 - t
             mt2 = mt * mt
             t2 = t * t
             a = mt2
             b = mt * t * 2
             c = t2
             x = (a * qx0) + (b * qx1) + (c * qx2)
             y = (a * qy0) + (b * qy1) + (c * qy2)
             z = (a * qz0) + (b * qz1) + (c * qz2)
         in (V3 x y z)

-- | Calculate the tangent vector to a bezier curve at a time.
tangent :: (Eq a, Fractional a) => Bezier a -> a -> V3 a
tangent = evalQuad . deriv

tangentCPH :: (Ord a, Fractional a) => CPH a -> CPH a -> a -> V3 a
tangentCPH c1 c2 = tangent (fromCPH (\ _ _ -> id) c1 c2)

-- | Calculate the normal vector to a bezier curve at a time.
normal :: (Eq a, Epsilon a, Floating a) => Bezier a -> a -> V3 a
normal bezier t =
  let tanVec@(V3 a b c) = tangent bezier t
  in if a /= (negate b)
     then normalize $ tanVec `cross` (V3 c c (negate $ a + b))
     else normalize $ tanVec `cross` (V3 (negate $ b + c) a a)
