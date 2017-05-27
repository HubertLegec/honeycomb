module UiUtils where
import Converters


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