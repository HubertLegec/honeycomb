module Converters where
import Types

convertCharToField :: Char -> Field
convertCharToField '.' = Nothing
convertCharToField c = Just c

convertStringToRow :: String -> Row
convertStringToRow "" = []
convertStringToRow (x:xs) = (convertCharToField x) : convertStringToRow xs

convertPlasterToHoneycomb :: Plaster -> HoneyComb
convertPlasterToHoneycomb (Plaster []) = []
convertPlasterToHoneycomb (Plaster (x:xs)) = (convertStringToRow x) : convertPlasterToHoneycomb (Plaster xs)

convertFieldToChar :: Field -> Char -> Char
convertFieldToChar Nothing e = e
convertFieldToChar (Just x) _ = x

convertRowToString :: Row -> String
convertRowToString [] = ""
convertRowToString (x:xs) = (convertFieldToChar x '.') : convertRowToString xs

convertHoneycombToPlaster :: HoneyComb -> Plaster
convertHoneycombToPlaster [] = Plaster []
convertHoneycombToPlaster hc = Plaster [convertRowToString x | x <- hc]