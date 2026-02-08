{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeOperators #-}

module Main (main) where

import Lib
import Diagrams.Prelude hiding (trace)
import Diagrams.Backend.SVG
import Effectful
import System.Directory (createDirectoryIfMissing)
import Data.Char (toLower)
import Data.List (nub, sortOn, groupBy, intercalate)
import Data.Function (on)

main :: IO ()
main = do
  createDirectoryIfMissing True "out"
  
  -- Generate SVG for each flag and collect metadata for index
  flagData <- mapM processFlag allCountryFlags
  
  -- Generate index.html
  let html = generateIndex flagData
  writeFile "out/index.html" html
  
  putStrLn $ "Generated " ++ show (length flagData) ++ " flag(s) and index.html"

-- | Process a single flag: render SVG and extract metadata
processFlag :: Flag (Sourced : Construction : '[]) -> IO (String, String, String, String, [SourcedElement], [ConstructionElement])
processFlag flag = do
  let isoLower = map toLower (flagIsoCode flag)
      svgFile = isoLower ++ ".svg"
      svgPath = "out/" ++ svgFile
      
  -- Render the SVG (run Sourced first since it's outer in the effect stack)
  let diagram = runPureEff $ runConstructionSVG $ runSourcedPure $ flagDesign flag
  renderSVG svgPath (mkWidth 300) diagram
  
  -- Get description (use runConstructionPure for non-Diagram results)
  let description = runPureEff $ runConstructionPure $ runSourcedPure $ flagDescription flag
  
  -- Collect sources from design and description
  let (_, designSources) = runPureEff $ runConstructionPure $ runSourcedCollect $ flagDesign flag
  let (_, descSources) = runPureEff $ runConstructionPure $ runSourcedCollect $ flagDescription flag
  let allSources = nub (designSources ++ descSources)
  
  -- Collect construction operations
  let (_, constructions) = runPureEff $ runConstructionCollect $ runSourcedPure $ flagDesign flag
  
  putStrLn $ "Generated " ++ svgFile ++ " (" ++ flagName flag ++ ")"
  
  pure (svgFile, flagName flag, description, flagIsoCode flag, allSources, constructions)

-- | Generate the index.html content
generateIndex :: [(String, String, String, String, [SourcedElement], [ConstructionElement])] -> String
generateIndex flags = unlines
  [ "<!DOCTYPE html>"
  , "<html lang=\"en\">"
  , "<head>"
  , "  <meta charset=\"UTF-8\">"
  , "  <meta name=\"viewport\" content=\"width=device-width, initial-scale=1.0\">"
  , "  <title>Constructible Flags</title>"
  , "  <style>"
  , "    body { font-family: sans-serif; max-width: 1200px; margin: 0 auto; padding: 20px; }"
  , "    table { border-collapse: collapse; width: 100%; }"
  , "    th, td { border: 1px solid #ddd; padding: 12px; text-align: left; vertical-align: top; }"
  , "    th { background-color: #f4f4f4; }"
  , "    img { max-width: 150px; height: auto; }"
  , "    ul { margin: 0; padding-left: 20px; }"
  , "    .elements { font-size: 0.9em; color: #666; }"
  , "  </style>"
  , "</head>"
  , "<body>"
  , "  <h1>Constructible Flags</h1>"
  , "  <table>"
  , "    <thead>"
  , "      <tr>"
  , "        <th>Design</th>"
  , "        <th>Name</th>"
  , "        <th>Description</th>"
  , "        <th>Construction</th>"
  , "        <th>Sources</th>"
  , "      </tr>"
  , "    </thead>"
  , "    <tbody>"
  , concatMap flagRow flags
  , "    </tbody>"
  , "  </table>"
  , "</body>"
  , "</html>"
  ]
  where
    flagRow (svgFile, name, desc, _, sources, constructions) = unlines
      [ "      <tr>"
      , "        <td><a href=\"" ++ svgFile ++ "\"><img src=\"" ++ svgFile ++ "\" alt=\"" ++ escapeHtml name ++ " flag\"></a></td>"
      , "        <td>" ++ escapeHtml name ++ "</td>"
      , "        <td>" ++ escapeHtml desc ++ "</td>"
      , "        <td>" ++ formatConstructions constructions ++ "</td>"
      , "        <td>" ++ formatSources sources ++ "</td>"
      , "      </tr>"
      ]
    
    -- Format construction operations grouped by technique
    formatConstructions :: [ConstructionElement] -> String
    formatConstructions [] = "<em>None</em>"
    formatConstructions elems =
      let naturals = [n | ConstructionNatural n <- elems]
          rationals = [(num, denom) | ConstructionRational num denom <- elems]
          boxCenters = [dims | ConstructionBoxCenter dims <- elems]
          items = concat
            [ if null naturals then [] else [formatNaturals naturals]
            , if null rationals then [] else [formatRationals rationals]
            , if null boxCenters then [] else [formatBoxCenters boxCenters]
            ]
      in if null items
         then "<em>None</em>"
         else "<ul>" ++ concat items ++ "</ul>"
    
    formatNaturals :: [Int] -> String
    formatNaturals ns = 
      let unique = nub ns
      in "<li>Natural <span class=\"elements\">(" ++ intercalate ", " (map show unique) ++ ")</span></li>"
    
    formatRationals :: [(Int, Int)] -> String
    formatRationals rs = 
      let unique = nub rs
          formatRational (num, denom) = show num ++ "/" ++ show denom
      in "<li>Rational <span class=\"elements\">(" ++ intercalate ", " (map formatRational unique) ++ ")</span></li>"
    
    formatBoxCenters :: [(Double, Double)] -> String
    formatBoxCenters bs = 
      let unique = nub bs
          formatDouble :: Double -> String
          formatDouble x = 
            let rounded = fromIntegral (round x :: Int)
            in if abs (x - rounded) < 1e-10
               then show (round x :: Int)
               else show x
          formatBox (w, h) = "(" ++ formatDouble w ++ ", " ++ formatDouble h ++ ")"
      in "<li>Box Center <span class=\"elements\">(" ++ intercalate ", " (map formatBox unique) ++ ")</span></li>"
    
    -- Group elements by source and format
    formatSources :: [SourcedElement] -> String
    formatSources [] = "<em>None</em>"
    formatSources elements = 
      let grouped = groupBy ((==) `on` snd) $ sortOn (sourceKey . snd) elements
      in "<ul>" ++ concatMap formatSourceGroup grouped ++ "</ul>"
    
    -- Key for sorting sources (to group same sources together)
    sourceKey :: Source -> String
    sourceKey SourceHabitual = "0"
    sourceKey (SourceLaw title _) = "1" ++ title
    sourceKey (SourceAuthoritativeWebsite title _) = "2" ++ title
    sourceKey (SourcePublication title _) = "3" ++ title
    
    formatSourceGroup :: [SourcedElement] -> String
    formatSourceGroup [] = ""
    formatSourceGroup group@((_, src):_) = 
      let elementNames = map fst group
          elementsStr = "<span class=\"elements\">(" ++ escapeHtml (joinElements elementNames) ++ ")</span>"
      in formatSourceWithElements src elementsStr
    
    joinElements :: [String] -> String
    joinElements xs = intercalate ", " xs
    
    formatSourceWithElements :: Source -> String -> String
    formatSourceWithElements SourceHabitual elems = 
      "<li>Habitual practice " ++ elems ++ "</li>"
    formatSourceWithElements (SourceAuthoritativeWebsite title url) elems = 
      "<li><a href=\"" ++ escapeHtml url ++ "\">" ++ escapeHtml title ++ "</a> " ++ elems ++ "</li>"
    formatSourceWithElements (SourceLaw title url) elems = 
      "<li><a href=\"" ++ escapeHtml url ++ "\">" ++ escapeHtml title ++ "</a> (Law) " ++ elems ++ "</li>"
    formatSourceWithElements (SourcePublication title url) elems = 
      "<li><a href=\"" ++ escapeHtml url ++ "\">" ++ escapeHtml title ++ "</a> (Publication) " ++ elems ++ "</li>"
    
    escapeHtml :: String -> String
    escapeHtml = concatMap escapeChar
      where
        escapeChar '<' = "&lt;"
        escapeChar '>' = "&gt;"
        escapeChar '&' = "&amp;"
        escapeChar '"' = "&quot;"
        escapeChar c   = [c]
