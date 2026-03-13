{-# LANGUAGE GADTs #-}

module Flag.Construction.Layers
  ( ConstructionLayer (..),
    layerInputPoints,
    layerOutputPoints,
    pointDist,
    evalLayers,
    pruneLayers,
  )
where

import Data.Colour
import qualified Data.Set as S
import Flag.Construction.Geometry
import Flag.Construction.Types

-- | A single layer of construction output, capturing what was built at each step.
-- This is decoupled from any rendering backend. Each intersection step stores
-- its defining points (so data dependencies can be traced) and result points.
-- 'LayerLabel' is a transparent annotation: it carries the label name and the
-- point value but does not count as a construction step.
data ConstructionLayer
  = LayerIntersectLL
      Point
      -- | First line defining points
      Point
      Point
      -- | Second line defining points
      Point
      -- | Result point
      [Point]
  | LayerIntersectLC
      Point
      -- | Line defining points
      Point
      Point
      -- | Circle defining points (center, edge)
      Point
      -- | Result points
      [Point]
  | LayerIntersectCC
      Point
      -- | First circle (center, edge)
      Point
      Point
      -- | Second circle (center, edge)
      Point
      -- | Result points
      [Point]
  | LayerNGonVertex
      Point
      -- | Circle defining points (center, edge)
      Point
      -- | Result point
      [Point]
  | -- | A filled triangle
    LayerTriangle (Colour Double) Point Point Point
  | -- | A filled circle (center, edge)
    LayerCircle (Colour Double) Point Point
  | -- | content drawing with mask drawing applied
    LayerMasked MaskMode Drawing Drawing
  | -- | An external SVG overlay (path, center, edge)
    LayerSVGOverlay FilePath Point Point
  | -- | A named annotation on a point (not a construction step)
    LayerLabel String Point
  deriving (Show)

-- | The points consumed as inputs by a construction layer.
layerInputPoints :: ConstructionLayer -> [Point]
layerInputPoints (LayerIntersectLL lp1 lp2 lp3 lp4 _) = [lp1, lp2, lp3, lp4]
layerInputPoints (LayerIntersectLC lp1 lp2 cc ce _) = [lp1, lp2, cc, ce]
layerInputPoints (LayerIntersectCC c1 e1 c2 e2 _) = [c1, e1, c2, e2]
layerInputPoints (LayerNGonVertex c e _) = [c, e]
layerInputPoints (LayerTriangle _ p1 p2 p3) = [p1, p2, p3]
layerInputPoints (LayerCircle _ center edge) = [center, edge]
layerInputPoints (LayerMasked _ _ _) = []
layerInputPoints (LayerSVGOverlay _ center edge) = [center, edge]
layerInputPoints (LayerLabel _ _) = []

-- | The points produced as outputs by a construction layer.
layerOutputPoints :: ConstructionLayer -> [Point]
layerOutputPoints (LayerIntersectLL _ _ _ _ pts) = pts
layerOutputPoints (LayerIntersectLC _ _ _ _ pts) = pts
layerOutputPoints (LayerIntersectCC _ _ _ _ pts) = pts
layerOutputPoints (LayerNGonVertex _ _ pts) = pts
layerOutputPoints (LayerTriangle _ _ _ _) = []
layerOutputPoints (LayerCircle _ _ _) = []
layerOutputPoints (LayerMasked _ _ _) = []
layerOutputPoints (LayerSVGOverlay _ _ _) = []
layerOutputPoints (LayerLabel _ p) = [p]

-- | Euclidean distance (exported for rendering code that derives radii).
pointDist :: Point -> Point -> Number
pointDist = dist

-- | Prune a flat list of construction layers by removing:
--
--   * __Duplicate outputs__: a geometric layer whose output point(s) are
--     identical to those already produced by an earlier layer.  If the
--     duplicate layer happens to be a 'LayerLabel', the label is instead
--     kept (labels are cheap transparent annotations and can sensibly
--     point to a previously-computed point).
--
--   * __Dead computations__: a geometric layer (any layer that produces
--     output points but is not a fill or SVG layer) whose output point(s)
--     are never consumed as inputs by any later layer in the list, and
--     which are not referenced by any 'LayerLabel'.
--
-- Fill layers ('LayerTriangle', 'LayerCircle', 'LayerMasked',
-- 'LayerSVGOverlay') are always kept regardless, because they produce
-- visible output rather than intermediate points.
-- 'LayerLabel' annotations are also always kept.
pruneLayers :: [ConstructionLayer] -> [ConstructionLayer]
pruneLayers layers = dropDead (dedupOutputs layers)

-- ---------------------------------------------------------------------------
-- Pass 1: deduplicate outputs
-- ---------------------------------------------------------------------------

-- | Remove any geometric layer whose output points have all already been
-- produced by an earlier layer.  A 'LayerLabel' is never removed here
-- (it is a transparent annotation, not a geometric step).
dedupOutputs :: [ConstructionLayer] -> [ConstructionLayer]
dedupOutputs = go S.empty
  where
    go _ [] = []
    go seen (l : ls) =
      let outs = layerOutputPoints l
       in case l of
            -- Labels are transparent: always keep, never update the seen set
            LayerLabel {} -> l : go seen ls
            -- Fill / overlay layers produce no intermediate points: always keep
            _ | null outs -> l : go seen ls
            -- Geometric layer: drop if every output is already known
            _ ->
              let newOuts = filter (`S.notMember` seen) outs
               in if null newOuts
                    then go seen ls -- fully duplicate — drop it
                    else l : go (foldl (flip S.insert) seen outs) ls

-- ---------------------------------------------------------------------------
-- Pass 2: dead-computation elimination
-- ---------------------------------------------------------------------------

-- | Drop any geometric layer whose output points are never used as inputs
-- by any later layer and are not referenced by any 'LayerLabel'.
-- Fill / overlay layers are always kept.
dropDead :: [ConstructionLayer] -> [ConstructionLayer]
dropDead layers =
  let needed = neededPoints layers
   in filter (isNeeded needed) layers
  where
    -- A layer is needed if it is a fill/overlay (no output points),
    -- a label, or if at least one of its output points is needed.
    isNeeded :: S.Set Point -> ConstructionLayer -> Bool
    isNeeded _ LayerLabel {} = True
    isNeeded _ LayerTriangle {} = True
    isNeeded _ LayerCircle {} = True
    isNeeded _ LayerMasked {} = True
    isNeeded _ LayerSVGOverlay {} = True
    isNeeded needed l =
      any (`S.member` needed) (layerOutputPoints l)

    -- Collect all points that are transitively needed, working backwards
    -- from fill/overlay/label anchors.
    --
    -- A point is needed if:
    --   1. It is referenced by a label, OR
    --   2. It is consumed as an input by a fill/overlay layer, OR
    --   3. It is consumed as an input by a geometric layer that is itself
    --      needed (i.e. whose output points are in the needed set).
    --
    -- We compute this by iterating to a fixed point.
    neededPoints :: [ConstructionLayer] -> S.Set Point
    neededPoints ls = fixpoint seed ls
      where
        -- Seed: points directly referenced by labels or consumed by fills/overlays
        seed =
          S.fromList $
            [p | LayerLabel _ p <- ls]
              ++ concatMap anchorInputs ls

        anchorInputs :: ConstructionLayer -> [Point]
        anchorInputs l@LayerTriangle {} = layerInputPoints l
        anchorInputs l@LayerCircle {} = layerInputPoints l
        anchorInputs l@LayerMasked {} = layerInputPoints l
        anchorInputs l@LayerSVGOverlay {} = layerInputPoints l
        anchorInputs _ = []

        -- Expand: if any output of a geometric layer is needed, all of its
        -- inputs become needed too.  Repeat until stable.
        fixpoint :: S.Set Point -> [ConstructionLayer] -> S.Set Point
        fixpoint needed geomLayers =
          let newNeeded = foldl addIfOutputNeeded needed geomLayers
           in if newNeeded == needed
                then needed
                else fixpoint newNeeded geomLayers

        addIfOutputNeeded :: S.Set Point -> ConstructionLayer -> S.Set Point
        addIfOutputNeeded needed l =
          let outs = layerOutputPoints l
           in if not (null outs) && any (`S.member` needed) outs
                then foldl (flip S.insert) needed (layerInputPoints l)
                else needed

-- | Evaluate a construction arrow, producing the result and a list of
-- construction layers (one per construction step).
evalLayers :: FlagA a b -> a -> (b, [ConstructionLayer])
evalLayers (Arr _ f) x = (f x, [])
evalLayers (Compose f g) x =
  let (mid, l1) = evalLayers f x
      (res, l2) = evalLayers g mid
   in (res, l1 ++ l2)
evalLayers (First f) (a, c) =
  let (b, ls) = evalLayers f a
   in ((b, c), ls)
evalLayers (Par f g) (a, c) =
  let (b, l1) = evalLayers f a
      (d, l2) = evalLayers g c
   in ((b, d), l1 ++ l2)
evalLayers IntersectLL inp@((lp1, lp2), (lp3, lp4)) =
  let p = evalIntersectLL' inp
   in (p, [LayerIntersectLL lp1 lp2 lp3 lp4 [p]])
evalLayers IntersectLC inp@((lp1, lp2), (cc, ce)) =
  let (p1, p2) = evalIntersectLC' inp
   in ((p1, p2), [LayerIntersectLC lp1 lp2 cc ce [p1, p2]])
evalLayers IntersectCC inp@((c1, e1), (c2, e2)) =
  let (p1, p2) = evalIntersectCC' inp
   in ((p1, p2), [LayerIntersectCC c1 e1 c2 e2 [p1, p2]])
evalLayers (NGonVertex n k) inp@(c, e) =
  let p = evalNGonVertex n k inp
   in (p, [LayerNGonVertex c e [p]])
evalLayers (FillTriangle col) (p1, p2, p3) =
  (DrawTriangle col p1 p2 p3, [LayerTriangle col p1 p2 p3])
evalLayers (FillCircle col) (center, edge) =
  (DrawCircle col center (dist center edge), [LayerCircle col center edge])
evalLayers (MaskDrawing mode) (content, mask) =
  (DrawMasked mode content mask, [LayerMasked mode content mask])
evalLayers (OverlaySVG path) (center, edge) =
  (DrawSVGOverlay path center edge, [LayerSVGOverlay path center edge])
evalLayers (Group _ f) x = evalLayers f x
evalLayers (LabelPoint _) p = (p, [])
