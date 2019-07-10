module Shirts.Hawaiian
  ( hawaiian
  ) where

import           Data.Bool
import           Diagrams.Backend.Cairo.CmdLine
import qualified Diagrams.Color.XKCD as X
import           Diagrams.Prelude hiding (ix)
import           Halton

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
  ] # centerXY

flower2 :: Int -> Double -> Diagram B
flower2 n d = mconcat
  [ splotch 0.5 X.windowsBlue d # lw none
  , flowerSplotch n darkFlowerCol (d + 2) # scale 0.4
  , flowerSplotch n X.windowsBlue d
  ] # centerXY

flowers :: Int -> Diagram B
flowers n = mconcat $ do
  (i, h) <- zip [0..n] $ halton 2 5
  let p = (i * 7) `mod` 5 + 3
      z = (i * 1394) `mod` 100 < 65
      d = fromIntegral i * 0.37
      s = 1 + sin (fromIntegral i + d) * 0.5
  pure $ bool (flower2 p d) (flower d) z # scale s # translate (h * 100)


hawaiian :: Diagram B
hawaiian = mconcat
  [ flowers 100
  , orangeSplotches 50
  , roundedRect 200 200 0.1 # fillColor (darken 0.4 X.blueberry) # rectEnvelope (0^&0) (0^&5) # centerXY
  ] # centerXY # rectEnvelope ((-45) ^& (-45)) (100^&100)

orangeSplotches :: Int -> Diagram B
orangeSplotches n = mconcat $ do
  (i, h) <- zip [0..n] $ drop 19 $ halton 2 6
  let d = fromIntegral i * 0.37
      s = 3 + sin (fromIntegral i + d)
  pure $ splotch 3 X.brightOrange d # lw none # scale s # translate (h * 100)


