module Validation where
import Types

-- sprawdź poprawność plastra (liczba wierszy oraz liczba elementów w wierszu)
validateInputHoneycomb :: Honeycomb -> Bool
validateInputHoneycomb [] = False
validateInputHoneycomb (x:xs) = validateRow xs (length x) (length x) 1

-- sprawdź poprawność wiersza plastra
validateRow :: [Row] -> Int -> Int -> Int -> Bool
validateRow [] _ 0 _ = True
validateRow [] _ _ _ = error "Nieodpowiednia liczba wierszy plastra"
validateRow (x:xs) firstRowSize rowCounter oddEven | (length x) == (firstRowSize + oddEven) =
                                                               validateRow xs firstRowSize (rowCounter - 1) (abs (oddEven - 1))
                                                   | otherwise = error ("Niepoprawna liczba pol w " ++ (show (rowCounter + 2 - firstRowSize)) ++ " wierszu")