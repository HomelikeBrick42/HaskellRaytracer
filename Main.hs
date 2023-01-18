import Data.Maybe (fromMaybe)
import GHC.IO (unsafePerformIO)
import Graphics.Image
import System.Random (Random (random, randoms), RandomGen, mkStdGen, randomIO)
import Vector3

data Ray = Ray
  { rayOrigin :: Vector3 Double,
    rayDirection :: Vector3 Double
  }

data Material = Material
  { materialColor :: Vector3 Double,
    materialEmit :: Vector3 Double,
    materialReflect :: Double
  }

data Hit = Hit
  { hitPosition :: Vector3 Double,
    hitNormal :: Vector3 Double,
    hitDistance :: Double,
    hitMaterial :: Material
  }

class Intersect a where
  intersect :: Ray -> a -> Maybe (Either Hit Ray) -- either we hit something, or the ray has been moved/modified to go somewhere else

data Intersectable where
  Intersectable :: Intersect a => a -> Intersectable

instance Intersect Intersectable where
  intersect ray (Intersectable object) = intersect ray object

data Sphere = Sphere
  { sphereCenter :: Vector3 Double,
    sphereRadius :: Double,
    sphereMaterial :: Material
  }

instance Intersect Sphere where
  intersect (Ray origin direction) (Sphere center radius material) =
    if (discriminant < 0) || (distance <= 0)
      then Nothing
      else Just $ Left $ Hit position normal distance material
    where
      oc = origin - center
      a = sqrLengthVector3 direction
      half_b = dotVector3 oc direction
      c = sqrLengthVector3 oc - radius * radius
      discriminant = half_b * half_b - a * c
      distance =
        let t0 = (-half_b - sqrt discriminant) / a
            t1 = (-half_b + sqrt discriminant) / a
         in if (t0 < t1) && (t0 > 0)
              then t0
              else t1
      position = origin + direction * toVector3 distance
      normal = (position - center) / toVector3 radius

rayCast :: [Intersectable] -> Ray -> Maybe Hit
rayCast objects ray = foldr (nearest . intersect ray) Nothing objects
  where
    nearest :: Maybe (Either Hit Ray) -> Maybe Hit -> Maybe Hit
    nearest Nothing b = b
    nearest (Just (Left a)) Nothing = Just a
    nearest (Just (Left a)) (Just b) =
      Just $
        if hitDistance a < hitDistance b
          then a
          else b
    nearest (Just (Right a)) b = nearest (Left <$> rayCast objects a) b

rayTrace :: forall tg. RandomGen tg => tg -> [Intersectable] -> Ray -> Int -> Int -> (Vector3 Double, tg)
rayTrace rng objects ray maxDepth samplesPerBounce = trace rng ray maxDepth
  where
    trace :: tg -> Ray -> Int -> (Vector3 Double, tg)
    trace rng _ 0 = (Vector3 0 0 0, rng)
    trace rng ray depth
      | Just hit <- rayCast objects ray =
          let (color, rng') = sample rng hit depth samplesPerBounce
           in (color / toVector3 (fromIntegral samplesPerBounce), rng')
    trace rng ray _ = (skyColor $ rayDirection ray, rng)

    sample :: tg -> Hit -> Int -> Int -> (Vector3 Double, tg)
    sample rng hit depth 0 = (Vector3 0 0 0, rng)
    sample rng hit depth 1 = bounce rng hit depth
    sample rng hit depth samplesLeft =
      let (color, rng') = bounce rng hit depth
          (new_color, rng'') = sample rng' hit depth (samplesLeft - 1)
       in (color + new_color, rng'')

    bounce :: tg -> Hit -> Int -> (Vector3 Double, tg)
    bounce rng hit depth =
      let (randomDirection, rng') = randomVector3InDirection rng (hitNormal hit)
          material = hitMaterial hit
          ray' =
            Ray
              { rayOrigin = rayOrigin ray,
                rayDirection =
                  lerpVector3
                    randomDirection
                    (reflectVector3 (rayDirection ray) (hitNormal hit))
                    (materialReflect material)
              }
          (color, rng'') = trace rng' ray' (depth - 1)
       in (materialEmit material + materialColor material * color, rng'')

skyColor :: Vector3 Double -> Vector3 Double
skyColor direction = lerpVector3 downColor upColor $ vector3y (normalizeVector3 direction) * 0.5 + 0.5
  where
    upColor :: Vector3 Double
    upColor = Vector3 0.5 0.7 1.0
    downColor :: Vector3 Double
    downColor = Vector3 1 1 1

pixelColor ::
  [Intersectable] ->
  (Vector3 Double, Vector3 (Vector3 Double)) ->
  (Int, Int) ->
  (Int, Int) ->
  Vector3 Double
pixelColor
  objects
  (cameraPosition, Vector3 cameraRight cameraUp cameraForward)
  (width, height)
  (x, y) = fst $ rayTrace (mkStdGen (x + y * width)) objects ray 1000 100
    where
      aspect :: Double
      aspect = fromIntegral width / fromIntegral height
      uv :: Vector3 Double
      uv =
        Vector3
          ((fromIntegral x + 0.5) / fromIntegral width)
          ((fromIntegral y + 0.5) / fromIntegral height)
          0
      rayDirection :: Vector3 Double
      rayDirection =
        normalizeVector3 $
          (cameraRight * toVector3 (aspect * (vector3x uv * 2.0 - 1.0)))
            + (cameraUp * toVector3 (vector3y uv * 2.0 - 1.0))
            + cameraForward
      ray :: Ray
      ray = Ray cameraPosition rayDirection

image :: [Intersectable] -> (Vector3 Double, Vector3 (Vector3 Double)) -> (Int, Int) -> Image VU RGB Double
image objects camera (width, height) = makeImageR VU (height, width) pixel
  where
    vector3ToColor (Vector3 x y z) = PixelRGB x y z
    pixel (y, x) = vector3ToColor $ pixelColor objects camera (width, height) (x, height - y - 1)

main :: IO ()
main =
  let cameraPosition = Vector3 0 1 (-3)
      cameraDirection = Vector3 (Vector3 1 0 0) (Vector3 0 1 0) (Vector3 0 0 1)
      objects =
        [ Intersectable
            Sphere
              { sphereCenter = Vector3 0 1 0,
                sphereRadius = 1,
                sphereMaterial =
                  Material
                    { materialColor = Vector3 0.8 0.8 0.8,
                      materialEmit = Vector3 0 0 0,
                      materialReflect = 0
                    }
              },
          Intersectable
            Sphere
              { sphereCenter = Vector3 0 (-100000) 0,
                sphereRadius = 100000,
                sphereMaterial =
                  Material
                    { materialColor = Vector3 0.2 0.8 0.3,
                      materialEmit = Vector3 0 0 0,
                      materialReflect = 0
                    }
              }
        ]
   in writeImage "image.png" $ image objects (cameraPosition, cameraDirection) (640, 480)
