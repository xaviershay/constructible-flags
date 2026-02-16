module Flag.Construction.Optimize
    ( optimize
    ) where

import Data.List (foldl')
import qualified Data.Map.Strict as M
import qualified Data.Set as S
import Data.Maybe (fromMaybe)
import Data.Colour
import Data.Colour.SRGB (toSRGB, channelRed, channelGreen, channelBlue)

import Flag.Construction.Types

-- | Group triangles by fill colour, compute the
-- boundary edges (edges appearing only once), build adjacency and walk
-- cycles to emit `DrawPath` polygons. Non-triangle primitives are preserved.
optimize :: Drawing -> Drawing
optimize = buildDrawing . processGroups . flatten
  where
    -- Flatten Overlay tree into a list preserving order
    flatten :: Drawing -> [Drawing]
    flatten EmptyDrawing  = []
    flatten (Overlay a b) = flatten a ++ flatten b
    flatten d             = [d]

    -- Rebuild a Drawing from a list
    buildDrawing :: [Drawing] -> Drawing
    buildDrawing = foldr (<>) EmptyDrawing

    -- Process flattened list preserving order: only merge triangles that
    -- appear in contiguous runs. This avoids merging triangles separated by
    -- other primitives (e.g. DrawCircle) which may overlap visually.
    processGroups :: [Drawing] -> [Drawing]
    processGroups = go
      where
        go [] = []
        go (d:ds) = case d of
          DrawTriangle{} ->
            let (triRun, rest) = span isTriangle (d:ds)
            in  mergeTriangleRun triRun ++ go rest
          _ -> d : go ds

    isTriangle :: Drawing -> Bool
    isTriangle (DrawTriangle _ _ _ _) = True
    isTriangle _                       = False

    -- Merge triangles in a contiguous run by colour only (preserve order of
    -- the runs relative to other primitives).
    mergeTriangleRun :: [Drawing] -> [Drawing]
    mergeTriangleRun tris =
      let triList = [ (c, (p1,p2,p3)) | DrawTriangle c p1 p2 p3 <- tris ]
          groups = splitByColour triList
      in concatMap mergeGroup groups

    -- Split a contiguous triangle run into subruns of equal colour, preserving input order
    splitByColour :: [(Colour Double, (Point,Point,Point))] -> [(Colour Double, [(Point,Point,Point)])]
    splitByColour [] = []
    splitByColour ((c, tri):xs) = go c [tri] xs
      where
        go curr acc [] = [(curr, reverse acc)]
        go curr acc ((c', tri'):rest)
          | colourEq curr c' = go curr (tri':acc) rest
          | otherwise = (curr, reverse acc) : go c' [tri'] rest

    -- Merge all triangles of a colour into boundary cycles (DrawPath)
    mergeGroup :: (Colour Double, [(Point,Point,Point)]) -> [Drawing]
    mergeGroup (col, tris) =
      let edgeCounts = foldl' (\tbl e -> M.insertWith (+) e 1 tbl) M.empty (concatMap triEdges tris)
          boundaryEdges = M.keysSet $ M.filter (==1) edgeCounts
          adj = buildAdjacency boundaryEdges
          cycles = extractCycles boundaryEdges adj
      in map (DrawPath col) cycles

    -- Triangle edges as undirected normalized pairs
    triEdges :: (Point,Point,Point) -> [ (Point,Point) ]
    triEdges (a,b,c) = map normalizeEdge [(a,b),(b,c),(c,a)]

    normalizeEdge :: (Point,Point) -> (Point,Point)
    normalizeEdge (p1,p2) = if p1 <= p2 then (p1,p2) else (p2,p1)

    -- Build adjacency map from set of undirected boundary edges
    buildAdjacency :: S.Set (Point,Point) -> M.Map Point [Point]
    buildAdjacency es = S.foldl' (\tbl (a,b) -> M.insertWith (++) a [b] $ M.insertWith (++) b [a] tbl) M.empty es

    -- Extract cycles (closed ordered vertex lists, including returning to start)
    extractCycles :: S.Set (Point,Point) -> M.Map Point [Point] -> [[Point]]
    extractCycles edges0 adj0 = go edges0 []
      where
        go edges acc
          | S.null edges = reverse acc
          | otherwise =
              let (a,b) = S.findMin edges
                  (cyclePts, edges') = walkCycle a b edges adj0
              in  go edges' (cyclePts : acc)

    -- Walk starting edge (a,b), removing used edges from the set and returning ordered cycle with closing vertex
    walkCycle :: Point -> Point -> S.Set (Point,Point) -> M.Map Point [Point] -> ([Point], S.Set (Point,Point))
    walkCycle a b edges adj =
      let start = a
          firstEdge = normalizeEdge (a,b)
          edges1 = S.delete firstEdge edges
          goWalk prev curr es acc
            | curr == start = (reverse (start:acc), es)
            | otherwise =
                let neighs = fromMaybe [] (M.lookup curr adj)
                    -- pick next neighbour with an unused edge
                    mNext = findNext prev curr es neighs
                in  case mNext of
                      Nothing -> (reverse (curr:acc) ++ [start], es) -- malformed, close anyway
                      Just nxt -> let e = normalizeEdge (curr, nxt)
                                      es' = S.delete e es
                                  in goWalk curr nxt es' (curr:acc)
      in goWalk a b edges1 [a]

    -- Choose next neighbour that still has an unused boundary edge
    findNext :: Point -> Point -> S.Set (Point,Point) -> [Point] -> Maybe Point
    findNext prev curr es neighs =
      let candidates = filter (\nxt -> S.member (normalizeEdge (curr, nxt)) es) neighs
          -- prefer a neighbour that's not the previous vertex when possible
          filtered = filter (/= prev) candidates
      in case filtered of
           (x:_) -> Just x
           []    -> case candidates of
                      (x:_) -> Just x
                      []    -> Nothing

    -- compare colours by sRGB triple
    colourEq :: Colour Double -> Colour Double -> Bool
    colourEq c1 c2 =
      let (r1, g1, b1) = colourToTriple c1
          (r2, g2, b2) = colourToTriple c2
      in  r1 == r2 && g1 == g2 && b1 == b2

    colourToTriple :: Colour Double -> (Double, Double, Double)
    colourToTriple c =
      let rgb = toSRGB c
      in  (channelRed rgb, channelGreen rgb, channelBlue rgb)

