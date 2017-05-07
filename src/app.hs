import System.IO

data Plaster = Plaster [String] deriving (Read, Show)


validatePlaster :: Plaster -> Bool
validatePlaster (Plaster (x:xs)) = validatePlasterRow xs (length x) (length x) 1

validatePlasterRow :: [String] -> Int -> Int -> Int -> Bool
validatePlasterRow [] _ 0 _ = True
validatePlasterRow [] _ _ _ = error "Nieodpowiednia liczba wierszy plastra"
validatePlasterRow (x:xs) firstRowSize rowCounter oddEven | (length x) == (firstRowSize + oddEven) =
                                                               validatePlasterRow xs firstRowSize (rowCounter - 1) (abs (oddEven - 1))
                                                          | otherwise = error ("Niepoprawna liczba pol w wierszu " ++ (show rowCounter) ++ " od dolu")


main = do
        putStrLn "Podaj nazwe pliku z zagadka:"
        fileName <- getLine
        fileText <- readFile fileName
        let honeyComb :: Plaster
            honeyComb = read fileText :: Plaster
        putStrLn ("Poprawna struktura: " ++ show (validatePlaster honeyComb))
        putStrLn (show honeyComb)
