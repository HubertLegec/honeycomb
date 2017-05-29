module Converters where
import Types

-- zamień pole plastra z Char na typ Field - Maybe Char
convertCharToField :: Char -> Field
convertCharToField '.' = Nothing
convertCharToField c = Just c

-- zamień wiersz w postaci String na typ Row - [Field]
convertStringToRow :: String -> Row
convertStringToRow "" = []
convertStringToRow (x:xs) = (convertCharToField x) : convertStringToRow xs

-- zamień reprezentację plastra z postaci pliku wejściowego na typ Honeycomb
convertTextReprToHoneycomb :: TextHoneycomb -> Honeycomb
convertTextReprToHoneycomb (Plaster []) = []
convertTextReprToHoneycomb (Plaster (x:xs)) = (convertStringToRow x) : convertTextReprToHoneycomb (Plaster xs)

-- zamień typ Field na Char
convertFieldToChar :: Field -> Char -> Char
convertFieldToChar Nothing e = e
convertFieldToChar (Just x) _ = x

-- zamień typ Row ([Field]) na String
convertRowToString :: Row -> String
convertRowToString [] = ""
convertRowToString (x:xs) = (convertFieldToChar x '.') : convertRowToString xs

-- zamień typ Honeycomb na reprezentację zgodną z plikiem wejściowym
convertHoneycombToTextRepr :: Honeycomb -> TextHoneycomb
convertHoneycombToTextRepr [] = Plaster []
convertHoneycombToTextRepr hc = Plaster [convertRowToString x | x <- hc]