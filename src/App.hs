module App where
import Types
import Converters
import FileUtils
import UiUtils
import Validation
import Solver


findSolution :: HoneyComb -> IO ()
findSolution hc = do
                   let result = solve hc
                   if result == Nothing then
                      putStrLn "Nie udalo sie znalezc rozwiazania"
                   else
                      saveHoneycombToFile result


main :: IO()
main = do
        textHoneycomb <- loadHoneycombFromFile
        let honeycomb = convertTextReprToHoneycomb textHoneycomb
        let isValid = validateHoneycomb honeycomb
        putStrLn ("Wejsciowy plaster: ")
        showHoneycomb honeycomb
        if isValid then
            findSolution honeycomb
        else
            putStrLn "Plik wejsciowy ma niepoprawna strukture!!!"
