{-# LANGUAGE ApplicativeDo             #-}
{-# LANGUAGE FlexibleContexts          #-}
{-# LANGUAGE GADTs                     #-}
{-# LANGUAGE NoMonomorphismRestriction #-}

module Main where


-- $> :set args -w 256 -h 256 -o out.svg

-- $> main

import Diagrams.Prelude
import Diagrams.Backend.SVG.CmdLine
import Web.Suavemente


main :: IO ()
main = suavemente $ do
  r   <- realSlider "Red" 0 1 0.05 1
  g   <- realSlider "Green" 0 1 0.05 1
  b   <- realSlider "Blue" 0 1 0.05 1
  pure $ d $ sRGB r g b


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
