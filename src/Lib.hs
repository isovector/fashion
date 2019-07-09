{-# LANGUAGE ApplicativeDo             #-}
{-# LANGUAGE FlexibleContexts          #-}
{-# LANGUAGE GADTs                     #-}
{-# LANGUAGE NoMonomorphismRestriction #-}
{-# LANGUAGE TypeApplications          #-}

module Lib where

import Data.Bool
import Control.Monad.ST
import Data.STRef
import Diagrams.Core.Types
import Control.Monad
import Diagrams.Prelude
import Diagrams.Backend.SVG.CmdLine
import qualified Diagrams.Color.XKCD as X
import Web.Suavemente
import Data.Function (fix)

-- $> :set args -w 350 -h 350 -o out.svg

-- $> main


main :: IO ()
main = mainWith $ example
  -- r   <- realSlider "Red" 0 1 0.05 1
  -- g   <- realSlider "Green" 0 1 0.05 1
  -- b   <- realSlider "Blue" 0 1 0.05 1
  -- pure $ d $ sRGB r g b

wibble :: Double -> Deformation V2 V2 Double
wibble phase = Deformation $ \p ->
  (p^._x + 0.3 * cos ((p ^. _y + p ^. _x) * 2 + phase)) ^& (p^._y)

circles :: Path V2 Double
circles = circle 3

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
  ix <- [0 .. n - 1]
  let d = splotch 1 c (fromIntegral ix + px) # rotateBy (fromIntegral ix / fromIntegral n) # scaleX 2 # alignL # translateX 0.3
  pure $ d # rotateBy (fromIntegral ix / fromIntegral n)


oneHalton :: Int -> Int -> Double
oneHalton b ix = runST $ do
  f <- newSTRef @Double 1
  r <- newSTRef @Double 0
  i <- newSTRef ix

  fix $ \me -> do
    ival <- readSTRef i
    when (ival > 0) $ do
      modifySTRef f (/ fromIntegral b)
      fval <- readSTRef f
      ival <- readSTRef i
      modifySTRef r $ (+ (fval * fromIntegral (ival `mod` b)))
      modifySTRef i $ fromIntegral . floor . (/ fromIntegral b) . fromIntegral
      me

  readSTRef r


halton :: Int -> Int -> [V2 Double]
halton x y = drop 1 $ do
  i <- [0..]
  pure $ (oneHalton x i - 0.5) ^& (oneHalton y i - 0.5)



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
  (i, h) <- zip [0..n] $ halton 2 3
  let p = (i * 7) `mod` 5 + 3
      z = (i * 1394) `mod` 100 > 63
      d = fromIntegral i * 0.37
  pure $ bool (flower2 p d) (flower d) z # translate (h * 50)


example :: Diagram B
example = mconcat
  [ flowers 10
  , orangeSplotch 0.1 # translateY 10 # rectEnvelope (0^&0) (0^&30) # translateY 10 # rotateBy 0.09
  , roundedRect 100 100 0.1 # fillColor (darken 0.4 X.blueberry) # rectEnvelope (0^&0) (0^&5) # centerXY
  ]


orangeSplotch :: Double -> Diagram B
orangeSplotch d = mconcat
  [ splotch 3 X.brightOrange d # lw none # alignL # scale 3 # translateX 2.9 # translateY 3
  , splotch 3 X.brightOrange (d + 0.1) # lw none # scale 3 # translateY (-2)
  , splotch 3 X.brightOrange (d + 0.2) # lw none # alignR # scale 3 # translateX (-2.9)
  ] # scale (sin (d * tau) + 1) # rotateBy (cos d)

sq n = square n
        # lw none
        # fc purple

c col n = circle ((n/2))
        # fc col
        # lw none


pattern :: Colour Double -> Diagram B
pattern col =
  mconcat $ map block (reverse $ 1 : sizes)
    where
      -- sizes = []
      sizes   = map (\k -> 0.707106**k) [1..20]
      block n = c col n <> sq n
      -- TODO: Add in Eike's determination of 'x'.

sign :: Diagram B
sign =
    (content <> background)
              # font "Arial"
    ===
      pole
  where
    pole = rect 0.015 0.23
                # lw none
                # fc gray

    arrow = (((0 ^& 0) ~~ (1 ^& 0)
              # scale 0.04
              # centerXY
              # lw 1.1
              # alignR <> triangle 0.012 # lw none # fc black # rotateBy (-1/4) # alignL)
              # alignL <> triangle 0.012 # lw none # fc black # rotateBy (1/4) # alignR)
              # centerXY


    t = text "40" # scale 0.015
    number = (t <> circle 0.018 # lw 1.2 # lc black # fc yellow)
              # scale 1.2

    stName = (vsep 1.4 [ text "GERTRUDE", text "STREET" ])
              # scale 0.008

    content = ((number # centerXY === strutY 0.005 === arrow)
                === strutY 0.01 === stName
              ) # centerXY

    background = roundedRect 0.08 0.11 0.01
                  # lc black
                  # lw 1
                  # fc yellow


d col = ( sign
      # translateX (-0.05)
      # translateY (-0.05)
      # scale 1.5
      <> pattern col
      )
