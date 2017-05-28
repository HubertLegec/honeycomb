module UiUtils where

import Types

import Data.List

honeyCombFieldToString :: Field -> String
honeyCombFieldToString Nothing = "-"
honeyCombFieldToString (Just c) = [c]

honeyCombRowToString :: Row -> Bool -> String
honeyCombRowToString row pad = 
    (if pad then " " else "") ++ intercalate " " (map honeyCombFieldToString row)

honeyCombToStringImpl :: [Row] -> Bool -> String
honeyCombToStringImpl [] _ = ""
honeyCombToStringImpl (head:rest) pad =
    (honeyCombRowToString head pad) ++ "\n" ++ (honeyCombToStringImpl rest (not pad))


honeyCombToString :: HoneyComb -> String
honeyCombToString h =
    honeyCombToStringImpl h True