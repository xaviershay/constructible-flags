import Distribution.Simple
import System.Directory
import System.FilePath
import Data.Char (isSpace)
import Data.List (isPrefixOf, intercalate)
import Control.Monad

main :: IO ()
main = do
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
        let hsFiles = filter ((== ".hs") . takeExtension) files
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

toLowerFirst :: String -> String
toLowerFirst "" = ""
toLowerFirst (c:cs) = toLower c : cs

toLower :: Char -> Char
toLower ch
	| 'A' <= ch && ch <= 'Z' = toEnum (fromEnum ch + 32)
	| otherwise = ch
