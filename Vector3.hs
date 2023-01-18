module Vector3 where

import System.Random (Random (random), RandomGen)

data Vector3 a = Vector3
  { vector3x :: a,
    vector3y :: a,
    vector3z :: a
  }
  deriving (Eq, Show)

toVector3 :: a -> Vector3 a
toVector3 e = Vector3 e e e

mapVector3 :: (a -> b) -> Vector3 a -> Vector3 b
mapVector3 f (Vector3 x y z) = Vector3 (f x) (f y) (f z)

dotVector3 :: Num a => Vector3 a -> Vector3 a -> a
dotVector3 (Vector3 x1 y1 z1) (Vector3 x2 y2 z2) = (x1 * x2) + (y1 * y2) + (z1 * z2)

sqrLengthVector3 :: Num a => Vector3 a -> a
sqrLengthVector3 v = dotVector3 v v

lengthVector3 :: Floating a => Vector3 a -> a
lengthVector3 v = sqrt $ sqrLengthVector3 v

normalizeVector3 :: (Floating a, Fractional a) => Vector3 a -> Vector3 a
normalizeVector3 v = v / toVector3 (lengthVector3 v)

lerpVector3 :: (Num a) => Vector3 a -> Vector3 a -> a -> Vector3 a
lerpVector3 v1 v2 t = v1 * toVector3 (1 - t) + v2 * toVector3 t

reflectVector3 :: (Num a) => Vector3 a -> Vector3 a -> Vector3 a
reflectVector3 d n = d - (toVector3 (2 * dotVector3 d n) * n)

randomVector3 :: forall tg. RandomGen tg => tg -> (Vector3 Double, tg)
randomVector3 rng =
  let (x, rng') = (random rng :: (Double, tg))
      (y, rng'') = (random rng' :: (Double, tg))
      (z, rng''') = (random rng'' :: (Double, tg))
   in ( Vector3
          (x * 2.0 - 1.0)
          (y * 2.0 - 1.0)
          (z * 2.0 - 1.0),
        rng'''
      )

randomVector3InDirection :: forall tg. RandomGen tg => tg -> Vector3 Double -> (Vector3 Double, tg)
randomVector3InDirection rng normal =
  let (direction, rng') = randomVector3 rng
   in ( if dotVector3 direction normal >= 0
          then direction
          else -direction,
        rng'
      )

instance Num a => Num (Vector3 a) where
  (+) (Vector3 x1 y1 z1) (Vector3 x2 y2 z2) = Vector3 (x1 + x2) (y1 + y2) (z1 + z2)
  (*) (Vector3 x1 y1 z1) (Vector3 x2 y2 z2) = Vector3 (x1 * x2) (y1 * y2) (z1 * z2)
  abs = mapVector3 abs
  signum = mapVector3 signum
  fromInteger n = toVector3 (fromInteger n)
  negate = mapVector3 negate

instance Fractional a => Fractional (Vector3 a) where
  fromRational r = toVector3 (fromRational r)
  (/) (Vector3 x1 y1 z1) (Vector3 x2 y2 z2) = Vector3 (x1 / x2) (y1 / y2) (z1 / z2)
