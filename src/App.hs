module App where
import Types
import Converters
import FileUtils
import UiUtils
import Validation
import Solver
import Data.Time


findSolution :: Honeycomb -> IO ()
findSolution hc = do
                   let result = solveOne hc
                   showResult result


main :: IO()
main = do
        textHoneycomb <- loadHoneycombFromFile
        let honeycomb = convertTextReprToHoneycomb textHoneycomb
        let isValid = validateInputHoneycomb honeycomb
        putStrLn ("Wejsciowy plaster: ")
        showHoneycomb honeycomb
        if isValid then
            findSolution honeycomb
        else
            putStrLn "Plik wejsciowy ma niepoprawna strukture!!!"
