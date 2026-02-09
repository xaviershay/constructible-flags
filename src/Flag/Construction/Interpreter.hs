{-# LANGUAGE GADTs #-}

module Flag.Construction.Interpreter
    ( Step(..)
    , steps
    , eval
    ) where

import Flag.Construction.Types
import Flag.Construction.Geometry

-- | A labeled construction step for introspection
data Step
  = StepIntersectLL
  | StepIntersectLC
  | StepIntersectCC
  | StepFillTriangle
  | StepFillCircle
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
steps (FillTriangle _) = [StepFillTriangle]
steps (FillCircle _)   = [StepFillCircle]
steps (Group _ f)      = steps f

-- | Evaluate a construction arrow to produce a concrete function.
-- This is one possible interpreter; others could generate SVG, trace
-- steps, validate, etc.
eval :: FlagA a b -> a -> b
eval (Arr _ f)        = f
eval (Compose f g)    = eval g . eval f
eval (First f)        = \(a, c) -> (eval f a, c)
eval (Par f g)        = \(a, c) -> (eval f a, eval g c)
eval IntersectLL      = evalIntersectLL'
eval IntersectLC      = evalIntersectLC'
eval IntersectCC      = evalIntersectCC'
eval (FillTriangle c) = \(p1, p2, p3) -> DrawTriangle c p1 p2 p3
eval (FillCircle c)   = \(center, edge) -> DrawCircle c center (dist center edge)
eval (Group _ f)      = eval f
