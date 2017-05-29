module UiUtils where

import Types

import Data.List

honeycombFieldToString :: Field -> String
honeycombFieldToString Nothing = "-"
honeycombFieldToString (Just c) = [c]

honeycombRowToString :: Row -> Bool -> Int -> String
honeycombRowToString row padRow padAll = 
    (replicate padAll ' ') ++ (if padRow then " " else "") ++ intercalate " " (map honeycombFieldToString row)

honeycombToStringImpl :: [Row] -> Bool -> Int -> String
honeycombToStringImpl [] _ _ = ""
honeycombToStringImpl (head:rest) padRow padAll =
    (honeycombRowToString head padRow padAll) ++ "\n" ++ (honeycombToStringImpl rest (not padRow) padAll)


honeycombToString :: Honeycomb -> Int -> String
honeycombToString h padAll =
    honeycombToStringImpl h True padAll


showHoneycomb :: Honeycomb -> IO ()
showHoneycomb h = do
    putStrLn "---- honeycomb ---"
    putStr (honeycombToString h 1)
    putStrLn "------------------"

showResult :: Maybe Honeycomb -> IO ()
showResult (Just hc) = do
    putStrLn "Rozwiazanie:"
    showHoneycomb hc
showResult _ =
    putStrLn "Nie udalo sie znalezc rozwiazania"