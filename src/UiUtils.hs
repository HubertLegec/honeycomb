module UiUtils where
import Converters
import Types


showRow row rowNum = do
                      if (rowNum `mod` 2 == 0) then
                        putStr " "
                      else
                        putStr ""
                      putStrLn (concat [((convertFieldToChar x '_') : " ") | x <- row])

showHc [] _ = return ()
showHc (x:xs) num = do
                     showRow x num
                     showHc xs (num + 1)

showHoneycomb h = do
                   putStrLn " ---- honeycomb ---"
                   showHc h 0
                   putStrLn " ------------------"

showResult :: Maybe Honeycomb -> IO()
showResult (Just hc) = do
                 putStrLn "Rozwiazanie:"
                 showHoneycomb hc
showResult _ = putStrLn "Nie udalo sie znalezc rozwiazania"