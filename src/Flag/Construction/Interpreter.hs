{-# LANGUAGE GADTs #-}

module Flag.Construction.Interpreter
  ( Step (..),
    StepCategory (..),
    StepDoc (..),
    stepDoc,
    steps,
    layerStep,
    eval,
    evalCollectNumbers,
    evalLabels,
  )
where

import Flag.Construction.FieldNumber (FieldNumber)
import Flag.Construction.Geometry
import Flag.Construction.Layers (ConstructionLayer (..))
import Flag.Construction.Types

-- | A labeled construction step for introspection
data Step
  = StepIntersectLL
  | StepIntersectLC
  | StepIntersectCC
  | StepNGonVertex
  | StepFillTriangle
  | StepFillCircle
  | StepMaskDrawing
  | StepSVGOverlay
  deriving (Show, Eq, Bounded, Enum)

-- | Whether a step is a geometric construction or a rendering primitive.
data StepCategory = Geometric | Rendering
  deriving (Eq, Ord, Show)

-- | Documentation record for a single construction step.
data StepDoc = StepDoc
  { stepCategory :: StepCategory,
    -- | Human-readable name, e.g. @"IntersectLL — Line ∩ Line"@
    stepTitle :: String,
    -- | KaTeX source string, e.g. @"$$\\text{─}\\!\\cap\\!\\text{─}$$"@
    stepSymbol :: String,
    -- | Prose description of what the step does.
    stepDesc :: String,
    -- | Type signature string, e.g. @"Input: ((p₁,p₂),(p₃,p₄)) → Point"@
    stepSig :: String
  }

-- | Documentation for every 'Step' variant.  This is the single source of
-- truth used by both the construction page and the per-flag step summary.
-- GHC's incomplete-pattern check ensures every new 'Step' variant must be
-- handled here before the project will build.
stepDoc :: Step -> StepDoc
stepDoc StepIntersectLL =
  StepDoc
    Geometric
    "IntersectLL \8212 Line \8745 Line"
    "\\text{\9472}\\!\\cap\\!\\text{\9472}"
    "Given two lines (each defined by a pair of points), finds their \
    \unique point of intersection. Errors when lines are parallel or \
    \if any line has length zero."
    "((p\8321, p\8322), (p\8323, p\8324))  \8594  Point"
stepDoc StepIntersectLC =
  StepDoc
    Geometric
    "IntersectLC \8212 Line \8745 Circle"
    "\\text{\9472}\\!\\cap\\!\\bigcirc"
    "Given a line (two points) and a circle (centre + edge point), \
    \finds both points where the line crosses the circle. Errors if line does \
    \not intersect circle, if line has length zero, or circle has radius zero."
    "((p\8321, p\8322), (centre, edge))  \8594  (Point, Point)"
stepDoc StepIntersectCC =
  StepDoc
    Geometric
    "IntersectCC \8212 Circle \8745 Circle"
    "\\bigcirc\\!\\cap\\!\\bigcirc"
    "Given two circles (each as centre + edge point), finds both points \
    \of intersection. Errors if circles do not intersect, or if either radius \
    \is zero."
    "((centre\8321, edge\8321), (centre\8322, edge\8322))  \8594  (Point, Point)"
stepDoc StepNGonVertex =
  StepDoc
    Geometric
    "NGonVertex \8212 Regular polygon vertex"
    "\\star"
    "Computes the k-th vertex of a regular n-gon inscribed in a given \
    \circle (centre + first vertex).  Always marks flag as non-constructible, \
    \so only use for n values that can\8217t be constructed with simpler primitives \
    \(e.g. n\8201=\8201 7, 9, 11 \8230)."
    "index \8594 size \8594 (centre, firstVertex)  \8594  Point"
stepDoc StepFillTriangle =
  StepDoc
    Rendering
    "FillTriangle \8212 Filled triangle"
    "\\blacktriangle"
    "Draws a solid-colour triangle from three points."
    "(p\8321, p\8322, p\8323)  \8594  Drawing"
stepDoc StepFillCircle =
  StepDoc
    Rendering
    "FillCircle \8212 Filled circle"
    "\\bullet"
    "Draws a solid-colour filled disc. The radius is derived from the \
    \Euclidean distance between the centre and an edge point, so no \
    \explicit radius value is needed."
    "(centre, edgePoint)  \8594  Drawing"
stepDoc StepMaskDrawing =
  StepDoc
    Rendering
    "MaskDrawing \8212 Masked or clipped drawing"
    "\\square"
    "Composites one drawing on top of another using a mask or clip. \
    \In mask mode, the mask drawing defines transparent regions. \
    \In clip mode, only the area inside the \
    \clip shapes is preserved."
    "maskMode \8594 (content :: Drawing, mask :: Drawing)  \8594  Drawing"
stepDoc StepSVGOverlay =
  StepDoc
    Rendering
    "SVGOverlay \8212 External SVG overlay"
    "+"
    "Composites an external SVG file (e.g. an emblem or coat of arms) \
    \on top of the generated flag.  The overlay is positioned and \
    \scaled using a centre point and an edge point.  The contents of \
    \the overlay are not further analysed."
    "filePath \8594 (centre, edgePoint)  \8594  Drawing"

-- | Convert a 'ConstructionLayer' to its corresponding 'Step', if it
-- represents a geometric construction step.  Drawing primitives
-- ('LayerTriangle', 'LayerCircle', 'LayerMasked', 'LayerSVGOverlay')
-- and labels do not count as construction steps and return 'Nothing'.
layerStep :: ConstructionLayer -> Maybe Step
layerStep LayerIntersectLL {} = Just StepIntersectLL
layerStep LayerIntersectLC {} = Just StepIntersectLC
layerStep LayerIntersectCC {} = Just StepIntersectCC
layerStep LayerNGonVertex {} = Just StepNGonVertex
layerStep LayerTriangle {} = Nothing
layerStep LayerCircle {} = Nothing
layerStep LayerMasked {} = Nothing
layerStep LayerSVGOverlay {} = Nothing
layerStep LayerLabel {} = Nothing

-- | Extract the flat list of geometric construction steps, in order.
-- Ignores structural wiring ('Arr', 'First', etc.) and only reports
-- meaningful geometric operations.
steps :: FlagA a b -> [Step]
steps (Arr _ _) = []
steps (Compose f g) = steps f ++ steps g
steps (First f) = steps f
steps (Par f g) = steps f ++ steps g
steps IntersectLL = [StepIntersectLL]
steps IntersectLC = [StepIntersectLC]
steps IntersectCC = [StepIntersectCC]
steps (NGonVertex _ _) = [StepNGonVertex]
steps (FillTriangle _) = [StepFillTriangle]
steps (FillCircle _) = [StepFillCircle]
steps (MaskDrawing _) = [StepMaskDrawing]
steps (OverlaySVG _) = [StepSVGOverlay]
steps (Group _ f) = steps f
steps (LabelPoint _) = []

-- | Evaluate a construction arrow to produce a concrete function.
-- This is one possible interpreter; others could generate SVG, trace
-- steps, validate, etc.
eval :: FlagA a b -> a -> b
eval (Arr _ f) = \a -> let b = f a in b `seq` b
eval (Compose f g) = eval g . eval f
eval (First f) = \(a, c) -> (eval f a, c)
eval (Par f g) = \(a, c) -> (eval f a, eval g c)
eval IntersectLL = evalIntersectLL'
eval IntersectLC = evalIntersectLC'
eval IntersectCC = evalIntersectCC'
eval (NGonVertex n k) = evalNGonVertex n k
eval (FillTriangle c) = \(p1, p2, p3) -> DrawTriangle c p1 p2 p3
eval (FillCircle c) = \(center, edge) -> DrawCircle c center (dist center edge)
eval (MaskDrawing mode) = \(content, mask) -> DrawMasked mode content mask
eval (OverlaySVG path) = \(center, edge) -> DrawSVGOverlay path center edge
eval (Group _ f) = eval f
eval (LabelPoint _) = id

-- | Evaluate a construction arrow, collecting all 'Number' values
-- produced by intersection operations (intermediate construction points).
evalCollectNumbers :: FlagA a b -> a -> (b, [FieldNumber])
evalCollectNumbers (Arr _ f) a = let b = f a in b `seq` (b, [])
evalCollectNumbers (Compose f g) a =
  let (b, r1) = evalCollectNumbers f a
      (c, r2) = evalCollectNumbers g b
   in (c, r1 ++ r2)
evalCollectNumbers (First f) (a, c) =
  let (b, rs) = evalCollectNumbers f a
   in ((b, c), rs)
evalCollectNumbers (Par f g) (a, c) =
  let (b, r1) = evalCollectNumbers f a
      (d, r2) = evalCollectNumbers g c
   in ((b, d), r1 ++ r2)
evalCollectNumbers IntersectLL input =
  let p@(x, y) = evalIntersectLL' input
   in (p, [x, y])
evalCollectNumbers IntersectLC input =
  let ps@((x1, y1), (x2, y2)) = evalIntersectLC' input
   in (ps, [x1, y1, x2, y2])
evalCollectNumbers IntersectCC input =
  let ps@((x1, y1), (x2, y2)) = evalIntersectCC' input
   in (ps, [x1, y1, x2, y2])
evalCollectNumbers (NGonVertex n k) input =
  let p@(x, y) = evalNGonVertex n k input
   in (p, [x, y])
evalCollectNumbers (FillTriangle c) (p1, p2, p3) =
  (DrawTriangle c p1 p2 p3, [])
evalCollectNumbers (FillCircle c) (center, edge) =
  (DrawCircle c center (dist center edge), [])
evalCollectNumbers (MaskDrawing mode) (content, mask) =
  (DrawMasked mode content mask, [])
evalCollectNumbers (OverlaySVG path) (center, edge) =
  (DrawSVGOverlay path center edge, [])
evalCollectNumbers (Group _ f) a = evalCollectNumbers f a
evalCollectNumbers (LabelPoint _) p = (p, [])

-- | Walk the construction DAG collecting all 'LabelPoint' annotations.
-- Returns a list of @(point, name)@ pairs in encounter order.
-- Reuses 'eval' to thread the intermediate values forward.
evalLabels :: FlagA a b -> a -> [(Point, String)]
evalLabels (Arr _ f) a = let b = f a in b `seq` []
evalLabels (Compose f g) a =
  let b = eval f a
   in evalLabels f a ++ evalLabels g b
evalLabels (First f) (a, _) = evalLabels f a
evalLabels (Par f g) (a, c) = evalLabels f a ++ evalLabels g c
evalLabels IntersectLL _ = []
evalLabels IntersectLC _ = []
evalLabels IntersectCC _ = []
evalLabels (NGonVertex _ _) _ = []
evalLabels (FillTriangle _) _ = []
evalLabels (FillCircle _) _ = []
evalLabels (MaskDrawing _) _ = []
evalLabels (OverlaySVG _) _ = []
evalLabels (Group _ f) a = evalLabels f a
evalLabels (LabelPoint name) p = [(p, name)]
