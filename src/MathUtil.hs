module MathUtil where

import Linear hiding (angle)

-- lens
import qualified Control.Lens.Getter as L

declination :: RealFloat a => V3 a -> a
declination (V3 x y z) = atan2 (sqrt $ x * x + y * y) z

saacos :: (Ord a, Floating a) => a -> a
saacos fac 
  | fac <= -1 = pi
  | fac >= 1 = 0
  | otherwise = acos fac

mulQtQt :: Num a => Quaternion a -> Quaternion a -> Quaternion a
mulQtQt (Quaternion q10 (V3 q11 q12 q13)) (Quaternion q20 (V3 q21 q22 q23)) =
  let t0 = q10 * q20 - q11 * q21 - q12 * q22 - q13 * q23
      t1 = q10 * q21 + q11 * q20 + q12 * q23 - q13 * q22
      t2 = q10 * q22 + q12 * q20 + q13 * q21 - q11 * q23
      t3 = q10 * q23 + q13 * q20 + q11 * q22 - q12 * q21
  in Quaternion t0 (V3 t1 t2 t3)

toTrackQuatZY :: (Epsilon a, RealFloat a) => V3 a -> Quaternion a
toTrackQuatZY v@(V3 x y z) =
  let axis = if abs x + abs y < (10 ^^ (-4 :: Int))
             then V3 1 x 0
             else V3 (negate y) x 0
      len = norm v
      co = z / len
      q = axisAngle axis (saacos co)
      mat = fromQuaternion q
      fp = mat L.^._z
      angle = -0.5 * atan2 (negate fp L.^._x) (negate fp L.^._y)
      co' = cos angle
      si' = (sin angle) / len
      q2 = Quaternion co' (v ^* si')
  in mulQtQt q q2
