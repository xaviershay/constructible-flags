module Lib
    ( frenchFlag
    ) where

import Diagrams.Prelude
import Diagrams.Backend.SVG

-- French flag: three vertical stripes (blue, white, red)
frenchFlag :: Diagram B
frenchFlag = blueStripe ||| whiteStripe ||| redStripe
  where
    blueStripe = rect 1 2 # fc blue # lw none
    whiteStripe = rect 1 2 # fc white # lw none
    redStripe = rect 1 2 # fc red # lw none
