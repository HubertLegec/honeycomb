module App where
import Types
import Converters
import FileUtils


validatePlaster :: Plaster -> Bool
validatePlaster (Plaster []) = False
validatePlaster (Plaster (x:xs)) = validatePlasterRow xs (length x) (length x) 1

validatePlasterRow :: [String] -> Int -> Int -> Int -> Bool
validatePlasterRow [] _ 0 _ = True
validatePlasterRow [] _ _ _ = error "Nieodpowiednia liczba wierszy plastra"
validatePlasterRow (x:xs) firstRowSize rowCounter oddEven | (length x) == (firstRowSize + oddEven) =
                                                               validatePlasterRow xs firstRowSize (rowCounter - 1) (abs (oddEven - 1))
                                                          | otherwise = error ("Niepoprawna liczba pol w wierszu " ++ (show rowCounter) ++ " od dolu")


main :: IO()
main = do
        plaster <- loadPlasterFromFile
        let honeycomb = convertPlasterToHoneycomb plaster
        putStrLn ("Poprawna struktura: " ++ show (validatePlaster plaster))
        putStrLn (show honeycomb)
        putStrLn (show (convertHoneycombToPlaster honeycomb))
