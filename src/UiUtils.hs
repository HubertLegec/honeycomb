module UiUtils where

import Types

import Data.List

honeycombFieldToString :: Field -> String
honeycombFieldToString Nothing = "-"
honeycombFieldToString (Just c) = [c]

honeycombRowToString :: Row -> Bool -> String
honeycombRowToString row pad = 
    (if pad then " " else "") ++ intercalate " " (map honeycombFieldToString row)

honeycombToStringImpl :: [Row] -> Bool -> String
honeycombToStringImpl [] _ = ""
honeycombToStringImpl (head:rest) pad =
    (honeycombRowToString head pad) ++ "\n" ++ (honeycombToStringImpl rest (not pad))


honeycombToString :: Honeycomb -> String
honeycombToString h =
    honeycombToStringImpl h True


showHoneycomb :: Honeycomb -> IO ()
showHoneycomb h = do
	putStrLn " ---- honeycomb ---"
	putStrLn (honeycombToString h)
	putStrLn " ------------------"

showResult :: Maybe Honeycomb -> IO ()
showResult (Just hc) = do
	putStrLn "Rozwiazanie:"
	showHoneycomb hc
showResult _ =
	putStrLn "Nie udalo sie znalezc rozwiazania"