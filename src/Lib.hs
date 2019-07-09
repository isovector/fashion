{-# LANGUAGE ApplicativeDo #-}

module Lib where

import           Data.Bool
import           Diagrams.Backend.SVG.CmdLine
import qualified Diagrams.Color.XKCD as X
import           Diagrams.Prelude hiding (ix)
import           Halton
-- import           Web.Suavemente

-- $> :set args -w 350 -h 350 -o out.svg

-- main


main :: IO ()
main = mainWith $ example
  -- r   <- realSlider "Red" 0 1 0.05 1
  -- g   <- realSlider "Green" 0 1 0.05 1
  -- b   <- realSlider "Blue" 0 1 0.05 1
  -- pure $ d $ sRGB r g b

wibble :: Double -> Deformation V2 V2 Double
wibble phase = Deformation $ \p ->
  (p^._x + 0.3 * cos ((p ^. _y + p ^. _x) * 2 + phase)) ^& (p^._y)

zoop :: Double -> Path V2 Double -> Path V2 Double
zoop p d = d # deform' 0.001 (wibble p) # rotateBy p

-- blue: darkishBlue
-- orange: brightOrange

darkFlowerCol :: AlphaColour Double
darkFlowerCol = darken 0.5 X.darkBlueGrey

splotch :: Double -> AlphaColour Double -> Double -> Diagram B
splotch sz c d = circle sz # zoop d # stroke # fillColor c

flowerSplotch :: Int -> AlphaColour Double ->  Double -> Diagram B
flowerSplotch n c px = rotateBy (cos $ fromIntegral n) $ bordering $ mconcat $ do
  ix <- fromIntegral <$> [0 .. n - 1]
  let d = splotch 1 c (ix + px) # rotateBy (ix / fromIntegral n) # scaleX 2 # alignL # translateX 0.3
  pure $ d # rotateBy (ix / fromIntegral n)



bordering :: Diagram B -> Diagram B
bordering d = d # lw none <> d # scale 1.2 # lw 5 # lineColor darkFlowerCol

flower :: Double -> Diagram B
flower d = mconcat
  [ splotch 1.5 darkFlowerCol d # lw none
  , splotch 3 X.windowsBlue (d + 0.2) # lw 5 # lineColor darkFlowerCol
  ]

flower2 :: Int -> Double -> Diagram B
flower2 n d = mconcat
  [ splotch 0.5 X.windowsBlue d # lw none
  , flowerSplotch n darkFlowerCol (d + 2) # scale 0.4
  , flowerSplotch n X.windowsBlue d
  ]

flowers :: Int -> Diagram B
flowers n = mconcat $ do
  (i, h) <- zip [0..n] $ halton 3 7
  let p = (i * 7) `mod` 5 + 3
      z = (i * 1394) `mod` 100 > 63
      d = fromIntegral i * 0.37
  pure $ bool (flower2 p d) (flower d) z # translate (h * 60)


example :: Diagram B
example = mconcat
  [ flowers 30
  , orangeSplotch 0.1 # translateY 10 # rectEnvelope (0^&0) (0^&30) # translateY 10 # rotateBy 0.09
  , roundedRect 100 100 0.1 # fillColor (darken 0.4 X.blueberry) # rectEnvelope (0^&0) (0^&5) # centerXY
  ]


orangeSplotch :: Double -> Diagram B
orangeSplotch d = mconcat
  [ splotch 3 X.brightOrange d # lw none # alignL # scale 3 # translateX 2.9 # translateY 3
  , splotch 3 X.brightOrange (d + 0.1) # lw none # scale 3 # translateY (-2)
  , splotch 3 X.brightOrange (d + 0.2) # lw none # alignR # scale 3 # translateX (-2.9)
  ] # scale (sin (d * tau) + 1) # rotateBy (cos d)

