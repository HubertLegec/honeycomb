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

-- zamień reprezentację plastra z postaci pliku wejściowego na typ HoneyComb
convertTextReprToHoneyComb :: TextHoneyComb -> HoneyComb
convertTextReprToHoneyComb (Plaster []) = []
convertTextReprToHoneyComb (Plaster (x:xs)) = (convertStringToRow x) : convertTextReprToHoneyComb (Plaster xs)

-- zamień typ Field na Char
convertFieldToChar :: Field -> Char -> Char
convertFieldToChar Nothing e = e
convertFieldToChar (Just x) _ = x

-- zamień typ Row ([Field]) na String
convertRowToString :: Row -> String
convertRowToString [] = ""
convertRowToString (x:xs) = (convertFieldToChar x '.') : convertRowToString xs

-- zamień typ HoneyComb na reprezentację zgodną z plikiem wejściowym
convertHoneyCombToTextRepr :: HoneyComb -> TextHoneyComb
convertHoneyCombToTextRepr [] = Plaster []
convertHoneyCombToTextRepr hc = Plaster [convertRowToString x | x <- hc]