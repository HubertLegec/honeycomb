module App where
import Types
import Converters
import FileUtils
import UiUtils
import Validation
import Solver
import Data.Time


findSolution :: HoneyComb -> IO ()
findSolution hc = do
                   start <- getCurrentTime
                   let result = solveOne hc
                   end <- getCurrentTime
                   putStr "Czas wykonania: "
                   print (diffUTCTime end start)
                   showResult result


main :: IO()
main = do
        textHoneyComb <- loadHoneyCombFromFile
        let honeycomb = convertTextReprToHoneyComb textHoneyComb
        let isValid = validateInputHoneyComb honeycomb
        putStrLn ("Wejsciowy plaster: ")
        showHoneyComb honeycomb
        if isValid then
            findSolution honeycomb
        else
            putStrLn "Plik wejsciowy ma niepoprawna strukture!!!"
