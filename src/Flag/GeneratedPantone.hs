module Flag.GeneratedPantone (generatedPantoneRGB, generatedPantoneSourceUrl, generatedPantoneList) where

generatedPantoneRGB :: String -> Maybe (Int, Int, Int)
generatedPantoneRGB "15-4225-TCX" = Just (111,168,210)
generatedPantoneRGB "200-C" = Just (186,11,47)
generatedPantoneRGB "342-C" = Just (2,102,71)
generatedPantoneRGB "356-C" = Just (1,123,57)
generatedPantoneRGB "485-C" = Just (220,39,31)
generatedPantoneRGB "RED-032-C" = Just (241,49,64)
generatedPantoneRGB "REFLEX-BLUE-C" = Just (1,20,139)
generatedPantoneRGB _ = Nothing

generatedPantoneSourceUrl :: String -> Maybe String
generatedPantoneSourceUrl "15-4225-TCX" = Just "https://www.pantone.com/media/color-finder/img/chips/pantone-color-chip-15-4225-tcx.webp"
generatedPantoneSourceUrl "200-C" = Just "https://www.pantone.com/media/color-finder/img/chips/pantone-color-chip-200-c.webp"
generatedPantoneSourceUrl "342-C" = Just "https://www.pantone.com/media/color-finder/img/chips/pantone-color-chip-342-c.webp"
generatedPantoneSourceUrl "356-C" = Just "https://www.pantone.com/media/color-finder/img/chips/pantone-color-chip-356-c.webp"
generatedPantoneSourceUrl "485-C" = Just "https://www.pantone.com/media/color-finder/img/chips/pantone-color-chip-485-c.webp"
generatedPantoneSourceUrl "RED-032-C" = Just "https://www.pantone.com/media/color-finder/img/chips/pantone-color-chip-red-032-c.webp"
generatedPantoneSourceUrl "REFLEX-BLUE-C" = Just "https://www.pantone.com/media/color-finder/img/chips/pantone-color-chip-reflex-blue-c.webp"
generatedPantoneSourceUrl _ = Nothing

generatedPantoneList :: [(String, (Int, Int, Int, String, String))]
generatedPantoneList = [
    ("15-4225-TCX", (111,168,210,"images/pantone/15-4225-TCX.webp","https://www.pantone.com/media/color-finder/img/chips/pantone-color-chip-15-4225-tcx.webp")),
    ("200-C", (186,11,47,"images/pantone/200-c.webp","https://www.pantone.com/media/color-finder/img/chips/pantone-color-chip-200-c.webp")),
    ("342-C", (2,102,71,"images/pantone/342-C.webp","https://www.pantone.com/media/color-finder/img/chips/pantone-color-chip-342-c.webp")),
    ("356-C", (1,123,57,"images/pantone/356-C.webp","https://www.pantone.com/media/color-finder/img/chips/pantone-color-chip-356-c.webp")),
    ("485-C", (220,39,31,"images/pantone/485-C.webp","https://www.pantone.com/media/color-finder/img/chips/pantone-color-chip-485-c.webp")),
    ("RED-032-C", (241,49,64,"images/pantone/RED-032-C.webp","https://www.pantone.com/media/color-finder/img/chips/pantone-color-chip-red-032-c.webp")),
    ("REFLEX-BLUE-C", (1,20,139,"images/pantone/REFLEX-BLUE-C.webp","https://www.pantone.com/media/color-finder/img/chips/pantone-color-chip-reflex-blue-c.webp"))
    ]
