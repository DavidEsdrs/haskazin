import Data.Char (isUpper, isDigit, isLower)

isSpecialChar :: Char -> Bool
isSpecialChar c = c `elem` "!@#$%^&*()_-+=<>?/[]{}|"

isValid pass = 
    length pass >= 8 && 
    any isUpper pass && 
    any isLower pass &&
    any isDigit pass &&
    any isSpecialChar pass