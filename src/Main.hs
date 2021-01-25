module Main where

type Vector3 = (Float, Float, Float)

add :: Vector3 -> Vector3 -> Vector3
add (x, y, z) (a, b, c) = (a + x, b + y, c + z)

sub :: Vector3 -> Vector3 -> Vector3
sub (x, y, z) (a, b, c) = (x - a, y - b, z - c)

squaredMagnitude :: Vector3 -> Float
squaredMagnitude (x, y, z) = x * x + y * y + z * z

magnitude :: Vector3 -> Float
magnitude v = sqrt (squaredMagnitude v)

scalarMult :: Vector3 -> Float -> Vector3
scalarMult (x, y, z) c = (c * x, c * y, c * z)

dot :: Vector3 -> Vector3 -> Float
dot (x, y, z) (a, b, c) = x * a + b * y + c * z

cross :: Vector3 -> Vector3 -> Vector3
cross (x, y, z) (a, b, c) = (b * z + c * y, - (a * z + c * x), a * y + b * x)

normalize :: Vector3 -> Vector3
normalize v
  | magnitude v /= 0 = scalarMult v (1 / magnitude v)
  | otherwise = (0, 0, 0)

neg :: Vector3 -> Vector3
neg (x, y, z) = (- x, - y, - z)

type Point3 = Vector3

type Direction3 = Vector3

type Time = Float

type Ray = (Point3, Direction3) -- base and direction

positionAtTime :: Ray -> Time -> Point3
positionAtTime (base, dir) t = base `add` scalarMult dir t

quadraticRoots :: Float -> Float -> Float -> [Float]
quadraticRoots a b c
  | discriminant >= 0.0 = [0.5 * (- b + sqrt discriminant), 0.5 * (- b - discriminant)]
  | otherwise = []
  where
    discriminant = b * b - 4 * a * c

boolXor :: Bool -> Bool -> Bool
boolXor True b = not b
boolXor False b = b

type Color = (Float, Float, Float)

red, green, blue, white, black, midgrey, nearlyWhite :: Color
red = (1.0, 0.0, 0.0)
green = (0.0, 1.0, 0.0)
blue = (0.0, 0.0, 1.0)
white = (1.0, 1.0, 1.0)
black = (0.0, 0.0, 0.0)
midgrey = (0.5, 0.5, 0.5)
nearlyWhite = (0.8, 0.8, 0.8)

scaleColor :: Color -> Float -> Color
scaleColor (r, g, b) k = (r * k, g * k, b * k)

addColor :: Color -> Color -> Color
addColor (r1, g1, b1) (r2, g2, b2) = (r1 + r2, g1 + g2, b1 + b2)

clamp :: Color -> Color
clamp (r, g, b) =
  let clampfloat f = max 0.0 (min 1.0 f)
   in (clampfloat r, clampfloat g, clampfloat b)

combineColor :: Color -> Color -> Color
combineColor (r1, g1, b1) (r2, g2, b2) = (r1 * r2, g1 * g2, b1 * b2)

flatred, shinyred, semishinygreen, shinywhite :: Point3 -> Material
flatred _ = (red, 0.0, 1.0)
shinyred _ = (red, 0.3, 0.9)
semishinygreen _ = (green, 0.5, 0.7)
shinywhite _ = (white, 0.3, 0.9)

checkeredMatt :: Point3 -> Material
checkeredMatt (x, y, z) =
  let xeven = even (truncate (x / 20.0))
      yeven = even (truncate (y / 20.0))
      zeven = even (truncate (z / 20.0))
   in if xeven `boolXor` yeven `boolXor` zeven
        then (white, 0.0, 1.0)
        else (black, 0.0, 1.0)

type Reflectivity = Float

type Diffuseness = Float

type Material = (Color, Reflectivity, Diffuseness)

type Normal = Vector3

type Radius = Float

data Shape
  = Sphere Point3 Radius (Point3 -> Material)
  | Plane Normal Float (Point3 -> Material)

type Intersection = (Normal, Point3, Ray, Material)

epsilon :: Float
epsilon = 0.001

intersect :: Ray -> Shape -> [(Time, Intersection)]
intersect ray@(base, dir) (Sphere centre rad materialfn) =
  let a = squaredMagnitude dir
      b = 2 * (dir `dot` (base `sub` centre))
      c = squaredMagnitude (base `sub` centre) - rad ^ 2
      times = filter (> epsilon) (quadraticRoots a b c)
      normalAtTime t = normalize (positionAtTime ray t `sub` centre)
      intersectionAtTime t = (normalAtTime t, positionAtTime ray t, ray, materialfn (positionAtTime ray t))
   in map (\t -> (t, intersectionAtTime t)) times
intersect ray@(base, dir) (Plane normal d materialfn) =
  let vd = normalize normal `dot` dir
      v0 = negate ((normalize normal `dot` base) + d)
   in if vd == 0
        then []
        else
          let t = v0 / vd
              hitpoint = positionAtTime ray t
           in if t > epsilon
                then [(t, (if (vd > 0) then (neg normal) else normal, hitpoint, ray, materialfn hitpoint))]
                else []

closest :: [(Time, Intersection)] -> Intersection
closest xs =
  let selectNearest (t1, i1) (t2, i2) = if t1 < t2 then (t1, i1) else (t2, i2)
   in snd (foldl selectNearest (head xs) (tail xs))

data Light
  = Directional Vector3 Color
  | Spotlight Point3 Color

backgroundColor :: Color
backgroundColor = black

lights :: [Light]
lights = [ Spotlight (100, -30, 0) nearlyWhite,
            Spotlight (-100, -100, 150) nearlyWhite]

main :: IO ()
main = putStrLn "Hello, Haskell!"