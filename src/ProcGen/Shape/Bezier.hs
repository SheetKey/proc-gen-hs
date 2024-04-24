{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE RecordWildCards #-}

module ProcGen.Shape.Bezier where

-- linear
import Linear

-- composition-prelude
import Control.Composition

class (Ord a, Floating a, Epsilon a) => Bezier b a | b -> a where
  type Deriv b
  eval :: b -> a -> (V3 a, a)
  deriv :: b -> Deriv b
  -- | Calculate the tangent vector to a bezier curve at a time.
  tangent :: Bezier (Deriv b) a => b -> a -> V3 a
  tangent = fst .* eval . deriv
  -- | Calculate the normal vector to a bezier curve at a time.
  normal :: Bezier (Deriv b) a => b -> a -> V3 a
  normal bezier t =
    let tanVec@(V3 a b c) = tangent bezier t
    in if a /= negate b
       then normalize $ tanVec `cross` V3 c c (negate $ a + b)
       else normalize $ tanVec `cross` V3 (negate $ b + c) a a
  safeEval :: b -> a -> (V3 a, a)
  safeEval b t = if t < 0 || t > 1 then error "'t' out of bounds."
                 else eval b t
  safeTangent :: Bezier (Deriv b) a => b -> a -> V3 a
  safeTangent b t = if t < 0 || t > 1 then error "'t' out of bounds."
                    else tangent b t
  safeNormal :: Bezier (Deriv b) a => b -> a -> V3 a
  safeNormal b t = if t < 0 || t > 1 then error "'t' out of bounds."
                   else normal b t

-- | A linear bezier is the derivative of a quadratic bezier.
data LinBezier a = LinBezier
  { lx0 :: a 
  , ly0 :: a
  , lz0 :: a
  , lx1 :: a
  , ly1 :: a
  , lz1 :: a
  , taperL :: a -> a
  }

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
  , taperQ :: a -> a
  }

-- | A cubic bezier curve. Useful for math.
data CubicBezier a = CubicBezier
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
  , taperC :: a -> a
  }

instance (Ord a, Floating a, Epsilon a) => Bezier (LinBezier a) a where
  type Deriv (LinBezier a) = a
  eval LinBezier {..} tVal = case tVal of
    0 -> (V3 lx0 ly0 lz0, taperL 0)
    1 -> (V3 lx1 ly1 lz1, taperL 1)
    t -> let mt = 1 - t
             x = (mt * lx0) + (t * lx1)
             y = (mt * ly0) + (t * ly1)
             z = (mt * lz0) + (t * lz1)
         in (V3 x y z, taperL t)
  deriv _ = 0
  
instance (Ord a, Floating a, Epsilon a) => Bezier (QuadBezier a) a where
  type Deriv (QuadBezier a) = LinBezier a
  eval QuadBezier {..} time =
    case time of
      0 -> (V3 qx0 qy0 qz0, taperQ 0)
      1 -> (V3 qx2 qy2 qz2, taperQ 1)
      t -> let mt = 1 - t
               mt2 = mt * mt
               t2 = t * t
               a = mt2
               b = mt * t * 2
               c = t2
               x = (a * qx0) + (b * qx1) + (c * qx2)
               y = (a * qy0) + (b * qy1) + (c * qy2)
               z = (a * qz0) + (b * qz1) + (c * qz2)
           in (V3 x y z, taperQ t)
  deriv QuadBezier {..} = 
    let lx0 = 2 * (qx1 - qx0)
        ly0 = 2 * (qy1 - qy0)
        lz0 = 2 * (qz1 - qz0)
        lx1 = 2 * (qx2 - qx1)
        ly1 = 2 * (qy2 - qy1)
        lz1 = 2 * (qz2 - qz1)
        taperL = taperQ
    in LinBezier {..}

instance (Ord a, Floating a, Epsilon a) => Bezier (CubicBezier a) a where
  type Deriv (CubicBezier a) = QuadBezier a
  eval CubicBezier {..} time =
    case time of
      0 -> (V3 cx0 cy0 cz0, taperC 0)
      1 -> (V3 cx3 cy3 cz3, taperC 1)
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
           in (V3 x y z, taperC t)
  deriv CubicBezier {..} =
    let qx0 = 3 * (cx1 - cx0)
        qy0 = 3 * (cy1 - cy0)
        qz0 = 3 * (cz1 - cz0)
        qx1 = 3 * (cx2 - cx1)
        qy1 = 3 * (cy2 - cy1)
        qz1 = 3 * (cz2 - cz1)
        qx2 = 3 * (cx3 - cx2)
        qy2 = 3 * (cy3 - cy2)
        qz2 = 3 * (cz3 - cz2)
        taperQ = taperC
    in QuadBezier {..}
