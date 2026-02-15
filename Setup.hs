import Distribution.Simple
import Distribution.Simple.Utils (installOrdinaryFiles)
import System.Directory
import System.FilePath
import Data.Char (isSpace, isDigit)
import Data.List (isPrefixOf, intercalate, sort, isInfixOf)
import Control.Monad

main :: IO ()
main = do
    putStrLn "Generating src/Flag/GeneratedPantone.hs..."
    generatePantoneModule
    putStrLn "Generating src/Flag/Registry.hs..."
    generateRegistry
    defaultMain

trim :: String -> String
trim = f . f
  where f = reverse . dropWhile isSpace

splitComma :: String -> [String]
splitComma s = map trim $ split s
	where
		split "" = []
		split xs = let (a, b) = break (==',') xs in a : case b of
				[] -> []
				(_:rest) -> split rest

generateRegistry :: IO ()
generateRegistry = do
    let countryDir = "src/Flag/Country"
        outFile = "src/Flag/Registry.hs"
    exists <- doesDirectoryExist countryDir
    unless exists $ putStrLn ("Warning: " ++ countryDir ++ " does not exist")
    when exists $ do
        files <- listDirectory countryDir
        let hsFiles = sort . filter ((== ".hs") . takeExtension) $ files
        entries <- forM hsFiles $ \f -> do
            let path = countryDir </> f
            content <- readFile path
            let ls = lines content
                modStart = dropWhile (not . ("module " `isPrefixOf`) . dropWhile isSpace) ls
            case modStart of
                [] -> do
                    putStrLn $ "Skipping " ++ path ++ ": no module declaration"
                    return Nothing
                _ -> do
                    let modLines = takeWhile (not . ("where" `isPrefixOf`) . dropWhile isSpace) modStart
                        modDecl = unwords $ map (dropWhile isSpace) modLines
                        rest = trim $ drop (length "module ") modDecl
                        moduleName = trim $ takeWhile (/='(') rest
                        exports = if '(' `elem` rest
                                  then trim $ takeWhile (/=')') $ tail $ dropWhile (/='(') rest
                                  else ""
                        exportNames = if null exports then [] else splitComma exports
                        exportName = if null exportNames
                                     then toLowerFirst (takeBaseName f)
                                     else head exportNames
                    return $ Just (moduleName, exportName)

        let pairs = [ (m,n) | Just (m,n) <- entries ]
            imports = map (\(m,n) -> "import " ++ m ++ " (" ++ n ++ ")") pairs
            names = map snd pairs
            header = unlines
                [ "{-# LANGUAGE DataKinds #-}"
                , "{-# LANGUAGE TypeOperators #-}"
                , ""
                , "module Flag.Registry"
                , "    ( allCountryFlags"
                ]
            exportsBlock = unlines $ map (\n -> "    , " ++ n) names
            headerClose = "    ) where\n"
            preImports = unlines
                [ "import Flag.Source (Sourced)"
                , "import Flag.Definition (Flag)"
                ]
            listBody = "allCountryFlags :: [Flag (Sourced : '[])]\nallCountryFlags =\n    [ " ++ intercalate "\n    , " names ++ "\n    ]\n"
            contentOut = header ++ exportsBlock ++ headerClose ++ "\n" ++ preImports ++ "\n" ++ unlines imports ++ "\n" ++ listBody
        createDirectoryIfMissing True (takeDirectory outFile)
        writeFile outFile contentOut
        putStrLn $ "Wrote " ++ outFile

 

-- Generate Haskell module from data/pantone.json so lookups are compiled in
generatePantoneModule :: IO ()
generatePantoneModule = do
    let inFile = "data/pantone.json"
        outFile = "src/Flag/GeneratedPantone.hs"
    exists <- doesFileExist inFile
    unless exists $ do
        putStrLn $ "Warning: " ++ inFile ++ " does not exist, writing empty mapping"
        createDirectoryIfMissing True (takeDirectory outFile)
        writeFile outFile $ unlines
            [ "module Flag.GeneratedPantone (generatedPantoneRGB, generatedPantoneList) where"
            , ""
            , "generatedPantoneRGB :: String -> Maybe (Int, Int, Int)"
            , "generatedPantoneRGB _ = Nothing"
            , ""
            , "generatedPantoneList :: [(String, (Int, Int, Int))]"
            , "generatedPantoneList = []"
            ]
    when exists $ do
        content <- readFile inFile
        let entries = parsePantoneRGBs content
            makeCase (k,(r,g,b)) = "generatedPantoneRGB " ++ show k ++ " = Just (" ++ show r ++ "," ++ show g ++ "," ++ show b ++ ")"
            cases = map makeCase entries
            listEntries = map (\(k,(r,g,b)) -> "(" ++ show k ++ ", (" ++ show r ++ "," ++ show g ++ "," ++ show b ++ "))") entries
            out = unlines $ concat
                [ ["module Flag.GeneratedPantone (generatedPantoneRGB, generatedPantoneList) where", ""]
                , ["generatedPantoneRGB :: String -> Maybe (Int, Int, Int)"]
                , cases
                , ["generatedPantoneRGB _ = Nothing", ""]
                , ["generatedPantoneList :: [(String, (Int, Int, Int))]"]
                , ["generatedPantoneList = ["]
                , (case listEntries of
                [] -> []
                xs -> map (\e -> "    " ++ e ++ ",") (init xs) ++ ["    " ++ last xs])
                , ["    ]"]
                ]
        createDirectoryIfMissing True (takeDirectory outFile)
        writeFile outFile out
        putStrLn $ "Wrote " ++ outFile

-- very small ad-hoc parser for our controlled JSON format
parsePantoneRGBs :: String -> [(String, (Int, Int, Int))]
parsePantoneRGBs src = go (lines src)
  where
    go [] = []
    go (l:ls) =
      case extractKey l of
        Nothing -> go ls
        Just key ->
          let (obj, rest) = break (\s -> "}" `isPrefixOf` dropWhile isSpace s) ls
              rgbLine = head $ filter ("\"rgb\"" `isInfixOf`) obj
              (r,g,b) = parseRGB rgbLine
           in (key, (r,g,b)) : go rest

    extractKey s =
      let t = dropWhile isSpace s
      in if not (null t) && head t == '"'
           then Just $ takeWhile (/= '"') $ tail t
           else Nothing

    parseRGB s =
      let after = dropWhile (/= '[') s
          nums = takeWhile (/= ']') $ tail after
          parts = map (read . filter (\c -> isDigit c || c == '-')) $ splitBy ',' nums
      in case parts of
           [a,b,c] -> (a,b,c)
           _ -> (0,0,0)

    splitBy c xs = case break (== c) xs of
                     (h, []) -> [h]
                     (h, _:_rest) -> h : splitBy c _rest

toLowerFirst :: String -> String
toLowerFirst "" = ""
toLowerFirst (c:cs) = toLower c : cs

toLower :: Char -> Char
toLower ch
	| 'A' <= ch && ch <= 'Z' = toEnum (fromEnum ch + 32)
	| otherwise = ch
