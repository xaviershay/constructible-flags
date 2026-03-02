{-# LANGUAGE GADTs #-}

module Flag.Construction.Interpreter
    ( Step(..)
    , steps
    , eval
    , evalCollectNumbers
    , evalLabels
    ) where

import Flag.Construction.Types
import Flag.Construction.Geometry
import Flag.Construction.FieldNumber (FieldNumber)

-- | A labeled construction step for introspection
data Step
  = StepIntersectLL
  | StepIntersectLC
  | StepIntersectCC
  | StepFillTriangle
  | StepFillCircle
  | StepFillCrescent
  | StepNGonVertex
  | StepSVGOverlay   -- ^ an external SVG overlay counts as a single cost
  deriving (Show, Eq)

-- | Extract the flat list of geometric construction steps, in order.
-- Ignores structural wiring ('Arr', 'First', etc.) and only reports
-- meaningful geometric operations.
steps :: FlagA a b -> [Step]
steps (Arr _ _)        = []
steps (Compose f g)    = steps f ++ steps g
steps (First f)        = steps f
steps (Par f g)        = steps f ++ steps g
steps IntersectLL      = [StepIntersectLL]
steps IntersectLC      = [StepIntersectLC]
steps IntersectCC      = [StepIntersectCC]
steps (NGonVertex _ _) = [StepNGonVertex]
steps (FillTriangle _) = [StepFillTriangle]
steps (FillCircle _)   = [StepFillCircle]
steps (FillCrescent _) = [StepFillCrescent]
steps (OverlaySVG _)   = [StepSVGOverlay]
steps (Group _ f)      = steps f
steps (LabelPoint _)   = []

-- | Evaluate a construction arrow to produce a concrete function.
-- This is one possible interpreter; others could generate SVG, trace
-- steps, validate, etc.
eval :: FlagA a b -> a -> b
eval (Arr _ f)        = \a -> let b = f a in b `seq` b
eval (Compose f g)    = eval g . eval f
eval (First f)        = \(a, c) -> (eval f a, c)
eval (Par f g)        = \(a, c) -> (eval f a, eval g c)
eval IntersectLL      = evalIntersectLL'
eval IntersectLC      = evalIntersectLC'
eval IntersectCC      = evalIntersectCC'
eval (NGonVertex n k) = evalNGonVertex n k
eval (FillTriangle c) = \(p1, p2, p3) -> DrawTriangle c p1 p2 p3
eval (FillCircle c)   = \(center, edge) -> DrawCircle c center (dist center edge)
eval (FillCrescent c) = \((outerCenter, outerEdge), (innerCenter, innerEdge)) ->
    DrawCrescent c outerCenter (dist outerCenter outerEdge) innerCenter (dist innerCenter innerEdge)
eval (OverlaySVG path) = \(center, edge) -> DrawSVGOverlay path center edge
eval (Group _ f)      = eval f
eval (LabelPoint _)   = id

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
  let ps@((x1,y1),(x2,y2)) = evalIntersectLC' input
  in (ps, [x1, y1, x2, y2])
evalCollectNumbers IntersectCC input =
  let ps@((x1,y1),(x2,y2)) = evalIntersectCC' input
  in (ps, [x1, y1, x2, y2])
evalCollectNumbers (NGonVertex n k) input =
  let p@(x, y) = evalNGonVertex n k input
  in (p, [x, y])
evalCollectNumbers (FillTriangle c) (p1, p2, p3) =
  (DrawTriangle c p1 p2 p3, [])
evalCollectNumbers (FillCircle c) (center, edge) =
  (DrawCircle c center (dist center edge), [])
evalCollectNumbers (FillCrescent c) ((outerCenter, outerEdge), (innerCenter, innerEdge)) =
  (DrawCrescent c outerCenter (dist outerCenter outerEdge) innerCenter (dist innerCenter innerEdge), [])
evalCollectNumbers (OverlaySVG path) (center, edge) =
  (DrawSVGOverlay path center edge, [])
evalCollectNumbers (Group _ f) a = evalCollectNumbers f a
evalCollectNumbers (LabelPoint _) p = (p, [])

-- | Walk the construction DAG collecting all 'LabelPoint' annotations.
-- Returns a list of @(point, name)@ pairs in encounter order.
-- Reuses 'eval' to thread the intermediate values forward.
evalLabels :: FlagA a b -> a -> [(Point, String)]
evalLabels (Arr _ f)        a     = let b = f a in b `seq` []
evalLabels (Compose f g)    a     = let b = eval f a
                                    in  evalLabels f a ++ evalLabels g b
evalLabels (First f)        (a,c) = evalLabels f a
evalLabels (Par f g)        (a,c) = evalLabels f a ++ evalLabels g c
evalLabels IntersectLL      _     = []
evalLabels IntersectLC      _     = []
evalLabels IntersectCC      _     = []
evalLabels (NGonVertex _ _) _     = []
evalLabels (FillTriangle _) _     = []
evalLabels (FillCircle _)   _     = []
evalLabels (FillCrescent _) _     = []
evalLabels (OverlaySVG _)   _     = []
evalLabels (Group _ f)      a     = evalLabels f a
evalLabels (LabelPoint name) p    = [(p, name)]
