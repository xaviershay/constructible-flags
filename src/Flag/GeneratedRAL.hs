module Flag.GeneratedRAL (generatedRALRGB, generatedRALSourceUrl, generatedRALList) where

generatedRALRGB :: String -> Maybe (Int, Int, Int)
generatedRALRGB "1003" = Just (249,169,0)
generatedRALRGB "1023" = Just (247,181,0)
generatedRALRGB "6029" = Just (0,111,61)
generatedRALRGB _ = Nothing

generatedRALSourceUrl :: String -> Maybe String
generatedRALSourceUrl "1003" = Just "https://qconv.com/en/colors/ral-1003"
generatedRALSourceUrl "1023" = Just "https://qconv.com/en/colors/ral-1023"
generatedRALSourceUrl "6029" = Just "https://qconv.com/en/colors/ral-6029"
generatedRALSourceUrl _ = Nothing

generatedRALList :: [(String, (Int, Int, Int, String, String))]
generatedRALList = [
    ("1003", (249,169,0,"images/ral/ral-1003.png","https://qconv.com/en/colors/ral-1003")),
    ("1023", (247,181,0,"images/ral/ral-1023.png","https://qconv.com/en/colors/ral-1023")),
    ("6029", (0,111,61,"images/ral/ral-6029.png","https://qconv.com/en/colors/ral-6029"))
    ]
