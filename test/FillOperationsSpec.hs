{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeOperators #-}

module FillOperationsSpec (fillOperationsTests) where

import Codec.Picture
import Data.Colour (Colour)
import Data.Colour.SRGB (sRGB24)
import Data.Colour.Names (red)
import Data.Ratio ((%))
import System.Directory (createDirectoryIfMissing, doesFileExist)
import System.FilePath ((</>))
import Test.Tasty
import Test.Tasty.HUnit

import TestImageUtils (diffImage, imagesEqual, writePngImage)
import Flag.Constructions
  ( fillTriangle,
    fillCircle,
    fillCrescent,
    fillRectangle,
    fillBox,
    fillStar5,
    fillStar5Inner,
    fillStar7x2,
    fillStar7x3,
    fillStar7Inner,
    fillStar12InnerC,
    fillStar16InnerC,
    ngonVertex,
    midpoint,
  )
import Flag.Construction.Interpreter (evalLabels)
import Flag.Construction.Tree (evalTree)
import Flag.Construction.Types (Point, Drawing, FlagA)
import Flag.Render.Backend (renderDrawing)
import Flag.Render.DebugV2 (writeConstructionJson, writeDebugViewer)
import Flag.Render.PNGBackend (PNGBackend (..))

-- | All fill operation tests with golden image comparison and debug viewer output
fillOperationsTests :: TestTree
fillOperationsTests =
  testGroup
    "Fill Operations"
    [ testGroup "fillTriangle"
        [ goldenFillTest "fillTriangle-equilateral" "Equilateral Triangle" $
            fillTriangle testColor
              `withInput` ((0, 0), (1, 0), (0.5, sqrt 3 / 2))
              `withSize` 600,
          goldenFillTest "fillTriangle-right" "Right Triangle" $
            fillTriangle testColor
              `withInput` ((0, 0), (1, 0), (0, 1))
              `withSize` 600,
          goldenFillTest "fillTriangle-isosceles" "Isosceles Triangle" $
            fillTriangle testColor
              `withInput` ((0, 0), (1, 0), (0.5, 1))
              `withSize` 600
        ],
      testGroup "fillCircle"
        [ goldenFillTest "fillCircle-unit" "Unit Circle" $
            fillCircle testColor
              `withInput` ((0, 0), (1, 0))
              `withSize` 600,
          goldenFillTest "fillCircle-offset" "Offset Circle" $
            fillCircle testColor
              `withInput` ((2, 3), (3, 3))
              `withSize` 600
        ],
      testGroup "fillCrescent"
        [ goldenFillTest "fillCrescent-simple" "Simple Crescent" $
            fillCrescent testColor
              `withInput` (((0, 0), (1, 0)), ((0.3, 0), (0.9, 0)))
              `withSize` 600,
          goldenFillTest "fillCrescent-offset" "Offset Crescent" $
            fillCrescent testColor
              `withInput` (((0, 0), (1, 0)), ((-0.3, 0), (0.7, 0)))
              `withSize` 600
        ],
      testGroup "fillRectangle"
        [ goldenFillTest "fillRectangle-square" "Unit Square" $
            fillRectangle testColor
              `withInput` ((0, 0), (1, 0), (1, 1), (0, 1))
              `withSize` 600,
          goldenFillTest "fillRectangle-wide" "Wide Rectangle" $
            fillRectangle testColor
              `withInput` ((0, 0), (2, 0), (2, 1), (0, 1))
              `withSize` 600
        ],
      testGroup "fillBox"
        [ goldenFillTest "fillBox-3x2" "3×2 Box" $
            fillBox testColor 3 2
              `withInput` ((0, 0), (1, 0))
              `withSize` 600,
          goldenFillTest "fillBox-4x3" "4×3 Box" $
            fillBox testColor 4 3
              `withInput` ((0, 0), (1, 0))
              `withSize` 600
        ],
      testGroup "fillStar5"
        [ goldenFillTest "fillStar5-unit" "{5/2} Pentagram" $
            fillStar5 testColor
              `withInput` ((0, 0), (1, 0))
              `withSize` 600
        ],
      testGroup "fillStar5Inner"
        [ goldenFillTest "fillStar5Inner-half" "{5/2} Inner Star (1/2)" $
            fillStar5Inner (1 % 2) testColor
              `withInput` ((0, 0), (1, 0))
              `withSize` 600,
          goldenFillTest "fillStar5Inner-third" "{5/2} Inner Star (1/3)" $
            fillStar5Inner (1 % 3) testColor
              `withInput` ((0, 0), (1, 0))
              `withSize` 600
        ],
      testGroup "fillStar7x2"
        [ goldenFillTest "fillStar7x2-unit" "{7/2} Heptagram" $
            fillStar7x2 testColor
              `withInput` ((0, 0), (1, 0))
              `withSize` 600
        ],
      testGroup "fillStar7x3"
        [ goldenFillTest "fillStar7x3-unit" "{7/3} Heptagram" $
            fillStar7x3 testColor
              `withInput` ((0, 0), (1, 0))
              `withSize` 600
        ],
      testGroup "fillStar7Inner"
        [ goldenFillTest "fillStar7Inner-half" "7-point Inner Star (1/2)" $
            fillStar7Inner (1 % 2) testColor
              `withInput` ((0, 0), (1, 0))
              `withSize` 600,
          goldenFillTest "fillStar7Inner-third" "7-point Inner Star (1/3)" $
            fillStar7Inner (1 % 3) testColor
              `withInput` ((0, 0), (1, 0))
              `withSize` 600
        ],
      testGroup "fillStar12InnerC"
        [ goldenFillTest "fillStar12InnerC-unit" "12-point Inner Star" $
            testStar12InnerC
        ],
      testGroup "fillStar16InnerC"
        [ goldenFillTest "fillStar16InnerC-unit" "16-point Inner Star" $
            testStar16InnerC
        ]
    ]

-- | Standard test color (bright red)
testColor :: Colour Double
testColor = sRGB24 220 50 50

-- | Test configuration for fillStar12InnerC (requires 3 points)
testStar12InnerC :: FillTestConfig ((Point, Point, Point))
testStar12InnerC =
  let o = (0, 0) :: Point
      ovEdge = (1, 0) :: Point
      -- Create inner edge at 1/2 radius
      ivEdge = (0.5, 0) :: Point
   in fillStar12InnerC testColor
        `withInput` (o, ivEdge, ovEdge)
        `withSize` 600

-- | Test configuration for fillStar16InnerC (requires 3 points)
testStar16InnerC :: FillTestConfig ((Point, Point, Point))
testStar16InnerC =
  let o = (0, 0) :: Point
      v0 = (1, 0) :: Point
      innerEdge = (0.6, 0) :: Point
   in fillStar16InnerC testColor
        `withInput` (o, innerEdge, v0)
        `withSize` 600

-- | Configuration for a fill operation test
data FillTestConfig a = FillTestConfig
  { ftcArrow :: FlagA a Drawing,
    ftcInput :: a,
    ftcSize :: Double
  }

-- | Create a test configuration from an arrow
withInput :: FlagA a Drawing -> a -> FillTestConfig a
withInput arrow input = FillTestConfig arrow input 600

-- | Set the render size
withSize :: FillTestConfig a -> Double -> FillTestConfig a
withSize config size = config {ftcSize = size}

-- | Paths for golden and debug output
goldenDir :: FilePath
goldenDir = "test/golden/fill-ops"

failureDir :: FilePath
failureDir = "test/failures/fill-ops"

debugDir :: FilePath
debugDir = "out/debug-v2"

-- | Create a golden test for a fill operation with debug viewer support
goldenFillTest :: (Show a) => String -> String -> FillTestConfig a -> TestTree
goldenFillTest name displayName config = testCase name $ do
  -- Create output directories
  createDirectoryIfMissing True goldenDir
  createDirectoryIfMissing True failureDir
  createDirectoryIfMissing True debugDir

  let goldenPath = goldenDir </> (name ++ ".png")
      tmpPath = failureDir </> (name ++ "-candidate.png")
      failurePath = failureDir </> (name ++ "-diff.png")
      svgWidth = ftcSize config

  -- Evaluate the construction
  let arrow = ftcArrow config
      input = ftcInput config
      (drawing, tree) = evalTree arrow input
      labels = evalLabels arrow input
      -- Use standard initial points for debug viewer
      initialPts = ((0, 0), (1, 0)) :: (Point, Point)

  -- Render to PNG
  renderDrawing PNGBackend tmpPath svgWidth drawing

  -- Write debug viewer JSON (using standard initial points)
  writeConstructionJson displayName name initialPts tree labels

  -- Compare with golden image
  goldenExists <- doesFileExist goldenPath
  if not goldenExists
    then do
      -- First run: create golden image
      imgE <- readPng tmpPath
      case imgE of
        Left err -> assertFailure $ "Failed reading generated PNG: " ++ err
        Right dyn ->
          writePngImage goldenPath dyn
            >> assertFailure
              ( "Golden image created at "
                  ++ goldenPath
                  ++ ". Review and commit to accept.\n"
                  ++ "View construction at: file://"
                  ++ debugDir
                  ++ "/index.html?flag="
                  ++ name
              )
    else do
      -- Compare images
      genE <- readPng tmpPath
      goldE <- readPng goldenPath
      case (genE, goldE) of
        (Right gen, Right gold) -> do
          let imgGen = convertRGBA8 gen
              imgGold = convertRGBA8 gold
          if imagesEqual imgGen imgGold
            then pure ()
            else do
              let diff = diffImage imgGold imgGen
              writePng failurePath diff
              assertFailure $
                "Image mismatch for "
                  ++ name
                  ++ ".\n"
                  ++ "Diff written to "
                  ++ failurePath
                  ++ "\n"
                  ++ "View construction at: file://"
                  ++ debugDir
                  ++ "/index.html?flag="
                  ++ name
        (Left e, _) -> assertFailure $ "Failed reading generated PNG: " ++ e
        (_, Left e) -> assertFailure $ "Failed reading golden PNG: " ++ e
