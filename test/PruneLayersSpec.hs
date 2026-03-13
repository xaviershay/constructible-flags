module PruneLayersSpec (pruneLayersTests) where

import Data.Colour (Colour)
import Data.Colour.SRGB (sRGB)
import Flag.Construction.Layers
  ( ConstructionLayer (..),
    pruneLayers,
  )
import Flag.Construction.Types (Number)
import Test.Tasty
import Test.Tasty.HUnit

-- ---------------------------------------------------------------------------
-- Shared geometry fixtures
-- ---------------------------------------------------------------------------

-- Points used to define circles and lines in test layers.
-- Integer literals resolve to Number (FieldNumber) via fromInteger.
p00, p10, p01, p11 :: (Number, Number)
p00 = (0, 0)
p10 = (1, 0)
p01 = (0, 1)
p11 = (1, 1)

-- A distinct far-away point, used as a "new" output fed into fills
pFar :: (Number, Number)
pFar = (2, 3)

-- A point that is never used as a defining input by any fixture layer —
-- safe to use as the output of a "dead" NGonVertex with no consumers.
pDead :: (Number, Number)
pDead = (7, 11)

-- A shared colour for fill layers
red :: Colour Double
red = sRGB 1 0 0

-- ---------------------------------------------------------------------------
-- Helpers to build layers concisely
-- ---------------------------------------------------------------------------

-- | A LayerNGonVertex whose defining circle is (p00, p10) and whose
-- claimed output is the given point.
ngonLayer :: (Number, Number) -> ConstructionLayer
ngonLayer p = LayerNGonVertex p00 p10 [p]

-- | A LayerIntersectLL using p00,p10 / p01,p11 as defining points and
-- claiming the given point as output.
intersectLayer :: (Number, Number) -> ConstructionLayer
intersectLayer p = LayerIntersectLL p00 p10 p01 p11 [p]

-- | A fill triangle that does not use any of our variable points —
-- useful as an always-present sentinel that must survive pruning.
sentinel :: ConstructionLayer
sentinel = LayerTriangle red p00 p10 p01

-- ---------------------------------------------------------------------------
-- show-based comparison helpers
-- ---------------------------------------------------------------------------

-- | Assert that two layer lists are equal by comparing their 'show' representations.
(@?=~) :: [ConstructionLayer] -> [ConstructionLayer] -> Assertion
actual @?=~ expected =
  show actual @?= show expected

infix 1 @?=~

-- | Assert that a layer is present in the list by 'show' comparison.
assertElem :: String -> ConstructionLayer -> [ConstructionLayer] -> Assertion
assertElem msg layer ls =
  assertBool
    (msg ++ "\n  looking for: " ++ show layer ++ "\n  in: " ++ show ls)
    (show layer `elem` map show ls)

-- | Assert that a layer is absent from the list by 'show' comparison.
assertNotElem :: String -> ConstructionLayer -> [ConstructionLayer] -> Assertion
assertNotElem msg layer ls =
  assertBool
    (msg ++ "\n  layer should be absent: " ++ show layer ++ "\n  in: " ++ show ls)
    (show layer `notElem` map show ls)

-- ---------------------------------------------------------------------------
-- Tests
-- ---------------------------------------------------------------------------

pruneLayersTests :: TestTree
pruneLayersTests =
  testGroup
    "pruneLayers"
    [ -- -----------------------------------------------------------------------
      -- Baseline: nothing to prune
      -- -----------------------------------------------------------------------
      testCase "single geometric layer whose output feeds a fill is preserved" $ do
        -- geom produces p10; fill consumes p10.  Nothing to prune.
        let geom = ngonLayer p10
            fill = LayerTriangle red p10 p01 p11
        pruneLayers [geom, fill] @?=~ [geom, fill],
      testCase "two layers with distinct outputs are both preserved" $ do
        -- geom1 → p10, geom2 → pFar; both feed the fill.
        let geom1 = ngonLayer p10
            geom2 = ngonLayer pFar
            fill = LayerTriangle red p10 pFar p01
        pruneLayers [geom1, geom2, fill] @?=~ [geom1, geom2, fill],
      testCase "fill layer with no geometric predecessors is preserved" $ do
        pruneLayers [sentinel] @?=~ [sentinel],
      -- -----------------------------------------------------------------------
      -- Duplicate-output elimination
      -- -----------------------------------------------------------------------
      testCase "duplicate output: second identical NGonVertex is dropped" $ do
        -- Both layers claim to produce p10.  The second is redundant.
        -- p10 feeds the fill, so the first is still needed.
        let first = ngonLayer p10
            dup = ngonLayer p10
            fill = LayerTriangle red p10 p01 p11
        pruneLayers [first, dup, fill] @?=~ [first, fill],
      testCase "duplicate output: the earlier layer is kept, not the later one" $ do
        let first = ngonLayer p10
            dup = ngonLayer p10
            fill = LayerTriangle red p10 p01 p11
            result = pruneLayers [first, dup, fill]
        assertElem "first layer is present" first result,
      testCase "duplicate output: non-duplicate between two duplicates is preserved" $ do
        -- first → p10, middle → pFar, dup → p10 again.
        -- fill uses both p10 and pFar.  dup should be dropped; middle kept.
        let first = ngonLayer p10
            middle = ngonLayer pFar
            dup = ngonLayer p10
            fill = LayerTriangle red p10 pFar p01
        pruneLayers [first, middle, dup, fill] @?=~ [first, middle, fill],
      testCase "duplicate output: LayerLabel on an already-seen point is kept" $ do
        -- A label pointing at p10 must survive even though p10 was already
        -- produced by an earlier geometric step.  Labels are transparent
        -- annotations and must never be removed by dedup.
        let geom = ngonLayer p10
            label = LayerLabel "vertex A" p10
            fill = LayerTriangle red p10 p01 p11
            result = pruneLayers [geom, label, fill]
        assertElem "label is present in result" label result,
      testCase "duplicate output: fill layer after a duplicate is still kept" $ do
        -- The dup is dropped but the fill that follows must survive.
        let first = ngonLayer p10
            dup = ngonLayer p10
            fill = LayerTriangle red p10 p01 p11
            result = pruneLayers [first, dup, fill]
        assertElem "fill layer survives" fill result
        length result @?= 2, -- first + fill; dup gone

      -- -----------------------------------------------------------------------
      -- Dead-computation elimination
      -- -----------------------------------------------------------------------
      testCase "dead computation: standalone geometric layer with no consumer is dropped" $ do
        -- A single NGonVertex whose output is never fed into anything and
        -- is not labeled.
        pruneLayers [ngonLayer p10] @?=~ [],
      testCase "dead computation: geometric layer consumed by a fill is kept" $ do
        -- geom produces p10 which a fill triangle consumes.
        let geom = ngonLayer p10
            fill = LayerTriangle red p10 p01 p11
        pruneLayers [geom, fill] @?=~ [geom, fill],
      testCase "dead computation: chain — geom1 feeds geom2, geom2 feeds fill" $ do
        -- geom1 produces p10; geom2 takes p10 as a defining input and produces pFar;
        -- fill uses pFar.  Both geometric layers must survive.
        let geom1 = ngonLayer p10
            geom2 = LayerIntersectLL p10 p01 p00 p11 [pFar]
            fill = LayerTriangle red pFar p01 p11
            result = pruneLayers [geom1, geom2, fill]
        assertElem "geom1 survives (its output feeds geom2)" geom1 result
        assertElem "geom2 survives (its output feeds the fill)" geom2 result,
      testCase "dead computation: labeled point is kept even without a downstream fill" $ do
        -- A point that has no fill consumer but is explicitly labeled should
        -- be retained — it appears in the debug viewer.
        let geom = ngonLayer p10
            label = LayerLabel "interesting point" p10
            result = pruneLayers [geom, label]
        assertElem "geometric layer is kept because its output is labeled" geom result,
      testCase "dead computation: unreferenced layer among useful ones is dropped" $ do
        -- dead produces pDead which nobody uses or labels;
        -- live produces pFar which the fill uses.
        -- pDead is chosen so it does not coincide with any defining input of
        -- any other fixture layer, preventing false "needed" propagation.
        let dead = ngonLayer pDead
            live = ngonLayer pFar
            fill = LayerTriangle red pFar p01 p11
            result = pruneLayers [dead, live, fill]
        assertNotElem "dead layer is absent" dead result
        assertElem "live layer is present" live result
        assertElem "fill layer is present" fill result,
      -- -----------------------------------------------------------------------
      -- Interaction: duplicate pass followed by dead-computation pass
      -- -----------------------------------------------------------------------
      testCase "combined: duplicate that is also dead — both copies removed" $ do
        -- Two identical NGonVertex layers producing p10; p10 is never consumed
        -- or labeled.  After dedup, one remains; after dead-elim, that too is
        -- gone.
        let first = ngonLayer p10
            dup = ngonLayer p10
        pruneLayers [first, dup] @?=~ [],
      testCase "combined: duplicate + dead leaves only the fill" $ do
        -- first and dup both produce p10; p10 never feeds the fill (which
        -- uses hard-coded points p00/p01/p11).
        let first = ngonLayer p10
            dup = ngonLayer p10
            fill = LayerTriangle red p00 p01 p11 -- doesn't use p10
            result = pruneLayers [first, dup, fill]
        result @?=~ [fill]
    ]
