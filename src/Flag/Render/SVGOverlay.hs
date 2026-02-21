{-# LANGUAGE FlexibleContexts #-}

module Flag.Render.SVGOverlay
    ( loadOverlays
    ) where

import Data.List (nub)
import qualified Data.Map.Strict as Map
import Data.Map.Strict (Map)
import Diagrams.Backend.SVG (B)
import Diagrams.Prelude (Diagram)
import Diagrams.TwoD.Input (loadImageEmbedded)

import Flag.Construction.Types (Drawing(..))

-- | Load all SVG overlay files referenced in a Drawing into a map from
-- path to Diagram. Files that fail to load are omitted with a warning.
loadOverlays :: Drawing -> IO (Map FilePath (Diagram B))
loadOverlays drawing = do
    let paths = nub (extractPaths drawing)
    pairs <- mapM loadOne paths
    pure (Map.fromList [(p, d) | (p, Just d) <- pairs])
  where
    loadOne path = do
        result <- loadImageEmbedded path
        case result of
            Left err -> do
                putStrLn $ "Warning: could not load SVG overlay " ++ show path ++ ": " ++ err
                pure (path, Nothing)
            Right diag -> pure (path, Just diag)

    extractPaths :: Drawing -> [FilePath]
    extractPaths EmptyDrawing = []
    extractPaths (Overlay a b) = extractPaths a ++ extractPaths b
    extractPaths (DrawSVGOverlay path _ _) = [path]
    extractPaths _ = []
