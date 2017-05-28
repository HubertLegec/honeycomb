module Utils where

import Control.Exception.Base

-- Zwraca, czy lista lista zawiera wyłącznie unikalne elementy
isListUnique :: (Eq a) => [a] -> Bool
isListUnique [] = True
isListUnique (x:xs) = notElem x xs && isListUnique xs

replace1 :: a -> Int -> [a] -> [a]
replace1 value x list = -- do
    assert (x < length list)
    take x list ++ [value] ++ drop (x + 1) list

replace2 :: a -> (Int, Int) -> [[a]] -> [[a]]
replace2 value (x, y) list = 
    replace1 (replace1 value x (list !! y)) y list