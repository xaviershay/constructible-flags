module Flag.Render.PNGBackend
  ( PNGBackend (..),
  )
where

import Flag.Render.Backend (RenderBackend (..))
import Flag.Render.SVGBuilderBackend (SVGBuilderBackend (..))
import System.Directory (getTemporaryDirectory)
import System.FilePath (replaceExtension, takeFileName, (</>))
import System.Process (callProcess)

-- | A rendering backend that produces PNG files by first rendering to an
-- intermediate SVG (via 'SVGBuilderBackend') and then converting with
-- @rsvg-convert@.
--
-- 'PNGBackend' is a unit type — no configuration is required.  The
-- intermediate SVG is placed in the system temporary directory
-- (via 'getTemporaryDirectory') and named after the base name of the
-- output path.
data PNGBackend = PNGBackend

instance RenderBackend PNGBackend where
  -- \| Render a 'Drawing' to a PNG file.
  --
  -- Delegates the full SVG pipeline (including overlay injection) to
  -- 'SVGBuilderBackend', writing to a temporary SVG file, then converts
  -- that SVG to PNG at @outPath@ using @rsvg-convert@.
  renderDrawing _ outPath svgWidth drawing = do
    tmpDir <- getTemporaryDirectory
    let tmpSvg = tmpDir </> replaceExtension (takeFileName outPath) "svg"
    renderDrawing SVGBuilderBackend tmpSvg svgWidth drawing
    callProcess "rsvg-convert" [tmpSvg, "-o", outPath]
