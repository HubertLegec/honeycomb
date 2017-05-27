module App where
import Types


validatePlaster :: Plaster -> Bool
validatePlaster (Plaster []) = False
validatePlaster (Plaster (x:xs)) = validatePlasterRow xs (length x) (length x) 1

validatePlasterRow :: [String] -> Int -> Int -> Int -> Bool
validatePlasterRow [] _ 0 _ = True
validatePlasterRow [] _ _ _ = error "Nieodpowiednia liczba wierszy plastra"
validatePlasterRow (x:xs) firstRowSize rowCounter oddEven | (length x) == (firstRowSize + oddEven) =
                                                               validatePlasterRow xs firstRowSize (rowCounter - 1) (abs (oddEven - 1))
                                                          | otherwise = error ("Niepoprawna liczba pol w wierszu " ++ (show rowCounter) ++ " od dolu")


convertCharToField :: Char -> Field
convertCharToField '.' = Nothing
convertCharToField c = Just c

convertStringToRow :: String -> Row
convertStringToRow "" = []
convertStringToRow (x:xs) = (convertCharToField x) : convertStringToRow xs

convertPlasterToHoneycomb :: Plaster -> HoneyComb
convertPlasterToHoneycomb (Plaster []) = []
convertPlasterToHoneycomb (Plaster (x:xs)) = (convertStringToRow x) : convertPlasterToHoneycomb (Plaster xs)


main :: IO()
main = do
        putStrLn "Podaj nazwe pliku z zagadka:"
        fileName <- getLine
        fileText <- readFile fileName
        let plaster :: Plaster
            plaster = read fileText :: Plaster
        let honeycomb :: HoneyComb
            honeycomb = convertPlasterToHoneycomb plaster
        putStrLn ("Poprawna struktura: " ++ show (validatePlaster plaster))
        putStrLn (show honeycomb)
