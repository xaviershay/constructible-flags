{-# LANGUAGE GADTs #-}

module Flag.Construction.Interpreter
    ( Step(..)
    , steps
    , eval
    , evalCollectRadicals
    ) where

import Flag.Construction.Types
import Flag.Construction.Geometry
import Flag.Construction.Radical (Radical)

-- | A labeled construction step for introspection
data Step
  = StepIntersectLL
  | StepIntersectLC
  | StepIntersectCC
  | StepFillTriangle
  | StepFillCircle
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
steps (OverlaySVG _)   = [StepSVGOverlay]
steps (Group _ f)      = steps f

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
eval (OverlaySVG path) = \(center, edge) -> DrawSVGOverlay path center edge
eval (Group _ f)      = eval f

-- | Evaluate a construction arrow, collecting all 'Radical' values
-- produced by intersection operations (intermediate construction points).
evalCollectRadicals :: FlagA a b -> a -> (b, [Radical])
evalCollectRadicals (Arr _ f) a = let b = f a in b `seq` (b, [])
evalCollectRadicals (Compose f g) a =
  let (b, r1) = evalCollectRadicals f a
      (c, r2) = evalCollectRadicals g b
  in (c, r1 ++ r2)
evalCollectRadicals (First f) (a, c) =
  let (b, rs) = evalCollectRadicals f a
  in ((b, c), rs)
evalCollectRadicals (Par f g) (a, c) =
  let (b, r1) = evalCollectRadicals f a
      (d, r2) = evalCollectRadicals g c
  in ((b, d), r1 ++ r2)
evalCollectRadicals IntersectLL input =
  let p@(x, y) = evalIntersectLL' input
  in (p, [x, y])
evalCollectRadicals IntersectLC input =
  let ps@((x1,y1),(x2,y2)) = evalIntersectLC' input
  in (ps, [x1, y1, x2, y2])
evalCollectRadicals IntersectCC input =
  let ps@((x1,y1),(x2,y2)) = evalIntersectCC' input
  in (ps, [x1, y1, x2, y2])
evalCollectRadicals (NGonVertex n k) input =
  let p@(x, y) = evalNGonVertex n k input
  in (p, [x, y])
evalCollectRadicals (FillTriangle c) (p1, p2, p3) =
  (DrawTriangle c p1 p2 p3, [])
evalCollectRadicals (FillCircle c) (center, edge) =
  (DrawCircle c center (dist center edge), [])
evalCollectRadicals (OverlaySVG path) (center, edge) =
  (DrawSVGOverlay path center edge, [])
evalCollectRadicals (Group _ f) a = evalCollectRadicals f a
