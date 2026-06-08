{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeOperators #-}

module AnimationSpec (animationTests) where

import Data.Colour.Names (red)
import Effectful (runPureEff)
import Flag.Construction.Layers (ConstructionLayer (..))
import Flag.Construction.Interpreter (eval)
import Flag.Construction.Tree
  ( ConstructionTree (..),
    evalTree,
    layerGroupPaths,
    pruneTree,
  )
import Flag.Construction.Types (Drawing (..), Point)
import Flag.Country.JPN (japan)
import Flag.Definition (Flag (..))
import Flag.Render.Animation
  ( AnimationConfig (..),
    Frame (..),
    buildFrames,
    defaultAnimationConfig,
    frameBBox,
    layersForAnimation,
    pathsForAnimation,
  )
import Flag.Render.Bounds (drawingBounds)
import Flag.Source (runSourcedPure)
import Test.Tasty
import Test.Tasty.HUnit

animationTests :: TestTree
animationTests =
  testGroup
    "Animation"
    [ testCase "buildFrames produces buildup + hold frames in that order" $ do
        let cfg = defaultAnimationConfig {acFramesPerStep = 1, acHoldFrames = 3}
            frames = buildFrames cfg simpleDrawing simpleLayers []
        length frames @?= length simpleLayers + 3
        let buildup = take (length simpleLayers) frames
            hold = drop (length simpleLayers) frames
        all isBuildup buildup @?= True
        all isHold hold @?= True,
      testCase "frame count = nLayers * framesPerStep + holdFrames" $ do
        let cfg = defaultAnimationConfig {acFramesPerStep = 4, acHoldFrames = 7}
            frames = buildFrames cfg simpleDrawing simpleLayers []
        length frames @?= length simpleLayers * 4 + 7,
      testCase "no frame has a non-finite bbox" $ do
        let cfg = defaultAnimationConfig {acFramesPerStep = 2, acHoldFrames = 5}
            frames = buildFrames cfg simpleDrawing simpleLayers []
        all (allFinite . frameBBox) frames @?= True,
      testCase "final frame bbox is centred on the final drawing bbox" $ do
        -- The last hold frame should have settled on the (aspect-locked,
        -- padded) final-drawing bbox.  We check that the centre of the last
        -- frame's bbox is very close to the centre of the unpadded drawing
        -- bbox; padding and aspect-lock are symmetric around the centre, so
        -- they cannot move it.
        let cfg = defaultAnimationConfig {acFramesPerStep = 2, acHoldFrames = 30}
            frames = buildFrames cfg simpleDrawing simpleLayers []
            (lx1, ly1, lx2, ly2) = frameBBox (last frames)
            Just (dx1, dy1, dx2, dy2) = drawingBounds simpleDrawing
            cxL = (lx1 + lx2) / 2
            cyL = (ly1 + ly2) / 2
            cxD = (dx1 + dx2) / 2
            cyD = (dy1 + dy2) / 2
        assertBool
          ("centre x mismatch: frame " ++ show cxL ++ " vs drawing " ++ show cxD)
          (abs (cxL - cxD) < 1e-6)
        assertBool
          ("centre y mismatch: frame " ++ show cyL ++ " vs drawing " ++ show cyD)
          (abs (cyL - cyD) < 1e-6),
      testCase "works on a real flag (japan)" $ do
        -- Smoke test: buildFrames should not crash on a real flag, and the
        -- frame count should match the documented formula.
        let flagArrow = runPureEff $ runSourcedPure $ flagDesign japan
            input = ((0, 0), (1, 0)) :: (Point, Point)
            drawing = eval flagArrow input
            (_, tree) = evalTree flagArrow input
            layers = layersForAnimation tree
            paths = pathsForAnimation tree
            cfg = defaultAnimationConfig {acFramesPerStep = 2, acHoldFrames = 4}
            frames = buildFrames cfg drawing layers paths
        assertBool "japan has at least one pruned layer" (not (null layers))
        length paths @?= length layers
        length frames @?= length layers * 2 + 4
        assertBool "all japan frame bboxes finite" (all (allFinite . frameBBox) frames),
      testCase "layerGroupPaths matches flattened pruned layers" $ do
        -- The path list returned by 'pathsForAnimation' must align 1:1
        -- with the layer list returned by 'layersForAnimation'.
        let flagArrow = runPureEff $ runSourcedPure $ flagDesign japan
            input = ((0, 0), (1, 0)) :: (Point, Point)
            (_, tree) = evalTree flagArrow input
            layers = layersForAnimation tree
            paths = pathsForAnimation tree
        length paths @?= length layers,
      testCase "layerGroupPaths captures nested group labels" $ do
        -- A synthetic tree with two nested groups should produce a path
        -- of ["outer", "inner"] for the enclosed layer.
        let leaf = TreeLayer (LayerTriangle red (0, 0) (1, 0) (0, 1))
            tree = [TreeGroup "outer" [TreeGroup "inner" [leaf]]]
        layerGroupPaths (pruneTree tree) @?= [["outer", "inner"]]
    ]

-- ---------------------------------------------------------------------------
-- Synthetic fixture: two red triangles forming a unit square
-- ---------------------------------------------------------------------------

simpleLayers :: [ConstructionLayer]
simpleLayers =
  [ LayerTriangle red (0, 0) (1, 0) (0, 1),
    LayerTriangle red (1, 0) (1, 1) (0, 1)
  ]

simpleDrawing :: Drawing
simpleDrawing =
  DrawTriangle red (0, 0) (1, 0) (0, 1)
    <> DrawTriangle red (1, 0) (1, 1) (0, 1)

-- ---------------------------------------------------------------------------
-- Helpers
-- ---------------------------------------------------------------------------

isBuildup :: Frame -> Bool
isBuildup BuildupFrame {} = True
isBuildup _ = False

isHold :: Frame -> Bool
isHold HoldFrame {} = True
isHold _ = False

allFinite :: (Double, Double, Double, Double) -> Bool
allFinite (a, b, c, d) = all (\x -> not (isNaN x || isInfinite x)) [a, b, c, d]
