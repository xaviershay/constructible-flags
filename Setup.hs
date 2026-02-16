import Distribution.Simple
import System.Directory
import System.FilePath
import Data.Char (isSpace)
import Data.List (isPrefixOf, intercalate, sort)
import Control.Monad
import qualified Data.ByteString.Lazy as BL
import Data.Aeson (Value(..), eitherDecode)
import qualified Data.Aeson.KeyMap as KM
import qualified Data.Aeson.Key as Key
import qualified Data.Text as T

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
            [ "module Flag.GeneratedPantone (generatedPantoneRGB, generatedPantoneSourceUrl, generatedPantoneList) where"
            , ""
            , "generatedPantoneRGB :: String -> Maybe (Int, Int, Int)"
            , "generatedPantoneRGB _ = Nothing"
            , ""
            , "generatedPantoneSourceUrl :: String -> Maybe String"
            , "generatedPantoneSourceUrl _ = Nothing"
            , ""
            , "generatedPantoneList :: [(String, (Int, Int, Int, String, String))]"
            , "generatedPantoneList = []"
            ]
    when exists $ do
        raw <- BL.readFile inFile
        entries <- case eitherDecode raw of
            Left err -> fail $ "Failed to parse " ++ inFile ++ ": " ++ err
            Right val -> return (parsePantoneEntries val)
        let makeRGBCase (k,(r,g,b,_,_)) = "generatedPantoneRGB " ++ show k ++ " = Just (" ++ show r ++ "," ++ show g ++ "," ++ show b ++ ")"
            makeSrcCase (k,(_,_,_,_chip,src)) = "generatedPantoneSourceUrl " ++ show k ++ " = Just " ++ show src
            casesRGB = map makeRGBCase entries
            casesSrc = map makeSrcCase entries
            listEntries = map (\(k,(r,g,b,chip,src)) -> "(" ++ show k ++ ", (" ++ show r ++ "," ++ show g ++ "," ++ show b ++ "," ++ show chip ++ "," ++ show src ++ "))") entries
            out = unlines $ concat
                [ ["module Flag.GeneratedPantone (generatedPantoneRGB, generatedPantoneSourceUrl, generatedPantoneList) where", ""]
                , ["generatedPantoneRGB :: String -> Maybe (Int, Int, Int)"]
                , casesRGB
                , ["generatedPantoneRGB _ = Nothing", ""]
                , ["generatedPantoneSourceUrl :: String -> Maybe String"]
                , casesSrc
                , ["generatedPantoneSourceUrl _ = Nothing", ""]
                , ["generatedPantoneList :: [(String, (Int, Int, Int, String, String))]"]
                , ["generatedPantoneList = ["]
                , (case listEntries of
                    [] -> []
                    xs -> map (\e -> "    " ++ e ++ ",") (init xs) ++ ["    " ++ last xs])
                , ["    ]"]
                ]
        createDirectoryIfMissing True (takeDirectory outFile)
        writeFile outFile out
        putStrLn $ "Wrote " ++ outFile

-- Parse the top-level JSON object into sorted entries
parsePantoneEntries :: Value -> [(String, (Int, Int, Int, String, String))]
parsePantoneEntries (Object obj) = sort
    [ (key, parseEntry val) | (k, val) <- KM.toList obj, let key = T.unpack (Key.toText k) ]
  where
    parseEntry (Object o) =
        let rgb = case KM.lookup (Key.fromString "rgb") o of
                Just (Array arr) -> case mapM toInt (foldr (:) [] arr) of
                    Just [r,g,b] -> (r,g,b)
                    _            -> (0,0,0)
                _ -> (0,0,0)
            chip = getString "chip" o
            src  = getString "sourceUrl" o
        in case rgb of (r,g,b) -> (r,g,b,chip,src)
    parseEntry _ = (0,0,0,"","")

    toInt (Number n) = Just (round n)
    toInt _          = Nothing

    getString field o = case KM.lookup (Key.fromString field) o of
        Just (String t) -> T.unpack t
        _               -> ""
parsePantoneEntries _ = []

toLowerFirst :: String -> String
toLowerFirst "" = ""
toLowerFirst (c:cs) = toLower c : cs

toLower :: Char -> Char
toLower ch
	| 'A' <= ch && ch <= 'Z' = toEnum (fromEnum ch + 32)
	| otherwise = ch
