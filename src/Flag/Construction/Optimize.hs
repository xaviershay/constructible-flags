module Flag.Construction.Optimize
    ( optimize
    ) where

import Data.Colour
import Data.Colour.SRGB (toSRGB, channelRed, channelGreen, channelBlue)

import Flag.Construction.Types

-- | Optimize a 'Drawing' by merging adjacent same-colour triangles that
-- share an edge into a single 'DrawPath'.  This reduces the number of SVG
-- elements and eliminates visible seams between triangles.
optimize :: Drawing -> Drawing
optimize = buildDrawing . mergeAll . flatten
  where
    -- Flatten Overlay tree into a list preserving order
    flatten :: Drawing -> [Drawing]
    flatten EmptyDrawing  = []
    flatten (Overlay a b) = flatten a ++ flatten b
    flatten d             = [d]

    -- Rebuild a Drawing from a list
    buildDrawing :: [Drawing] -> Drawing
    buildDrawing = foldr (<>) EmptyDrawing

    -- Repeatedly try to merge adjacent same-colour primitives
    mergeAll :: [Drawing] -> [Drawing]
    mergeAll ds =
      let ds' = mergePass ds
      in  if length ds' == length ds then ds' else mergeAll ds'

    -- One pass: try to merge each element with the next
    mergePass :: [Drawing] -> [Drawing]
    mergePass [] = []
    mergePass [x] = [x]
    mergePass (x:y:rest) = case tryMerge x y of
      Just merged -> mergePass (merged : rest)
      Nothing     -> x : mergePass (y : rest)

    -- Try to merge two same-colour primitives that share an edge
    tryMerge :: Drawing -> Drawing -> Maybe Drawing
    tryMerge a b = do
      (colA, ptsA) <- toVertices a
      (colB, ptsB) <- toVertices b
      if colourEq colA colB
        then mergePolygons colA ptsA ptsB
        else Nothing

    -- Extract colour and vertex list from a triangle or path
    toVertices :: Drawing -> Maybe (Colour Double, [Point])
    toVertices (DrawTriangle c p1 p2 p3) = Just (c, [p1, p2, p3])
    toVertices (DrawPath c pts)          = Just (c, pts)
    toVertices _                         = Nothing  -- DrawCircle etc. cannot merge

    -- Compare colours by their sRGB channel values
    colourEq :: Colour Double -> Colour Double -> Bool
    colourEq c1 c2 =
      let (r1, g1, b1) = colourToTriple c1
          (r2, g2, b2) = colourToTriple c2
      in  r1 == r2 && g1 == g2 && b1 == b2

    colourToTriple :: Colour Double -> (Double, Double, Double)
    colourToTriple c =
      let rgb = toSRGB c
      in  (channelRed rgb, channelGreen rgb, channelBlue rgb)

    -- Try to merge two polygons that share an edge.
    -- Finds a shared edge (consecutive vertices appearing in both polygons
    -- in reverse order) and splices the two vertex lists together.
    mergePolygons :: Colour Double -> [Point] -> [Point] -> Maybe Drawing
    mergePolygons col ptsA ptsB = tryEdges (edges ptsA) ptsA ptsB
      where
        tryEdges [] _ _ = Nothing
        tryEdges ((i, pA1, pA2):es) as bs =
          case findSharedEdge pA1 pA2 bs of
            Just j  -> Just (DrawPath col (splicePolygons as i bs j))
            Nothing -> tryEdges es as bs

    -- All edges of a polygon as (index, point, nextPoint)
    edges :: [Point] -> [(Int, Point, Point)]
    edges pts = [ (i, pts !! i, pts !! ((i + 1) `mod` length pts))
                | i <- [0 .. length pts - 1] ]

    -- Find edge (p2, p1) — reversed — in polygon, returning its start index
    findSharedEdge :: Point -> Point -> [Point] -> Maybe Int
    findSharedEdge p1 p2 pts =
      let n = length pts
          matches = [ j | j <- [0 .. n - 1]
                        , ptEq (pts !! j) p2
                        , ptEq (pts !! ((j + 1) `mod` n)) p1 ]
      in  case matches of
            (j:_) -> Just j
            []    -> Nothing

    ptEq :: Point -> Point -> Bool
    ptEq (x1, y1) (x2, y2) = x1 == x2 && y1 == y2

    -- Splice two polygons along a shared edge.
    -- Edge i..(i+1) in A is shared with edge j..(j+1) in B (reversed).
    -- Result: walk A up to i, then insert B's non-shared vertices, then
    -- continue A after i+1.
    splicePolygons :: [Point] -> Int -> [Point] -> Int -> [Point]
    splicePolygons as ai bs bj =
      let _na = length as
          nb = length bs
          -- A vertices: keep all except skip edge endpoint at (ai+1)
          aBefore = take (ai + 1) as
          aAfter  = drop (ai + 2) as ++ take ai as  -- wrap around, skip shared
          -- B vertices: start after the shared edge, walk around
          bInsert = [ bs !! ((bj + 2 + k) `mod` nb) | k <- [0 .. nb - 3] ]
      in  aBefore ++ bInsert ++ aAfter
