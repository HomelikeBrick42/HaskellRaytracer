module Vector3 where

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
