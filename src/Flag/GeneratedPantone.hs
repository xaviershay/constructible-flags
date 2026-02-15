module Flag.GeneratedPantone (generatedPantoneRGB, generatedPantoneList) where

generatedPantoneRGB :: String -> Maybe (Int, Int, Int)
generatedPantoneRGB "PMSRed032C" = Just (230,49,62)
generatedPantoneRGB "PMSReflexBlueC" = Just (16,11,136)
generatedPantoneRGB "PMS154225TCX" = Just (117,168,210)
generatedPantoneRGB "PMS342C" = Just (24,104,72)
generatedPantoneRGB "PMS485C" = Just (210,40,24)
generatedPantoneRGB _ = Nothing

generatedPantoneList :: [(String, (Int, Int, Int))]
generatedPantoneList = [
    ("PMSRed032C", (230,49,62)),
    ("PMSReflexBlueC", (16,11,136)),
    ("PMS154225TCX", (117,168,210)),
    ("PMS342C", (24,104,72)),
    ("PMS485C", (210,40,24))
    ]
