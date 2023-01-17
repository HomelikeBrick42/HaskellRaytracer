import Data.Maybe (fromMaybe)
import Graphics.Image
import Vector3

data Ray = Ray
  { rayOrigin :: Vector3 Double,
    rayDirection :: Vector3 Double
  }

data Hit = Hit
  { hitPosition :: Vector3 Double,
    hitNormal :: Vector3 Double,
    hitDistance :: Double,
    hitColor :: Vector3 Double
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
    sphereColor :: Vector3 Double
  }

instance Intersect Sphere where
  intersect (Ray origin direction) (Sphere center radius color) =
    if discriminant < 0 || distance < 0
      then Nothing
      else Just $ Left $ Hit position normal distance color
    where
      oc = origin - center
      a = sqrLengthVector3 direction
      half_b = dotVector3 oc direction
      c = sqrLengthVector3 oc - radius * radius
      discriminant = half_b * half_b - a * c
      distance =
        let t0 = (-half_b - sqrt discriminant) / a
            t1 = (-half_b + sqrt discriminant) / a
         in if t0 > 0
              then t0
              else t1
      position = origin + direction * toVector3 distance
      normal = (position - center) / toVector3 radius

trace :: [Intersectable] -> Ray -> Maybe Hit
trace objects ray = foldr (nearest . intersect ray) Nothing objects
  where
    nearest :: Maybe (Either Hit Ray) -> Maybe Hit -> Maybe Hit
    nearest Nothing b = b
    nearest (Just (Left a)) Nothing = Just a
    nearest (Just (Left a)) (Just b) =
      Just $
        if hitDistance a < hitDistance b
          then a
          else b
    nearest (Just (Right a)) b = nearest (Left <$> trace objects a) b

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
  (x, y) = maybe (skyColor rayDirection) hitColor (trace objects ray)
    where
      aspect :: Double
      aspect = fromIntegral width / fromIntegral height
      uv :: Vector3 Double
      uv =
        Vector3
          (fromIntegral x / fromIntegral width)
          (fromIntegral y / fromIntegral height)
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
  let cameraPosition = Vector3 0 0 0
      cameraDirection = Vector3 (Vector3 1 0 0) (Vector3 0 1 0) (Vector3 0 0 1)
      objects =
        [ Intersectable
            Sphere
              { sphereCenter = Vector3 0 0 3,
                sphereRadius = 1,
                sphereColor = Vector3 1 0 0
              }
        ]
   in writeImage "image.png" $ image objects (cameraPosition, cameraDirection) (640, 480)
