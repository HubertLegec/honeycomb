module Solver where

import Utils
import Types

import Data.List
import Data.Maybe

import Control.Exception.Base

import Debug.Trace
import UiUtils

-- Rozwiązuje podany plaster miodu
solveOne :: Honeycomb -> (Maybe Honeycomb)
solveOne h =
	trace ( (honeycombToString h) ) (
    if isSolved h then
            Just h
    else
        let allHoneycombs = [(coords, char, replaceHoneycomb h coords char) | coords <- getEmptyPointsSortedByNeighboursFillRatio h, char <- honeycombLetters] in
            let validHoneycombs = [replacedHoneycomb | (coords, char, replacedHoneycomb) <- allHoneycombs, validateHoneycomb replacedHoneycomb] in
                solveList validHoneycombs
    )

-- Pomocnicza funkcja próbująca po kolei rozwiązać każdy z podanych plastrów.
-- Kończ się w chwili znalezienia pierwszego rozwiązania
solveList :: [Honeycomb] -> (Maybe Honeycomb)
solveList [] = Nothing
solveList (h:tail) =
    case (solveOne h) of
        Just h -> Just h
        Nothing -> solveList tail


-- Zwraca listę wszystkich liter które mogą pojawić się na planszy
honeycombLetters :: [Char]
honeycombLetters = ['A' .. 'G']

-- Zwraca pole o podanych współrzędnych
getField :: Honeycomb -> Coords -> FieldWithCoords
getField h (Coords x y) = 
    (FieldWithCoords (Coords x y) (h !! y !! x))

-- Zwraca pole plastra o podanych współrzędnych X, Y liczonych od lewego górnego rogu.
-- Jeśli pole nie istnieje - zwraca Nothig
findFieldIfExists :: Honeycomb -> Coords -> Maybe FieldWithCoords
findFieldIfExists h (Coords x y) =
    if y >= 0 && y < length h then
        let row = h !! y in
        if x >= 0 && x < length row then
            let field = row !! x in
                Just (FieldWithCoords (Coords x y) field)
        else
            Nothing
    else
        Nothing

-- Zwraca listę wszystkich współrzędnych punktów w plastrze
getAllCoords :: Honeycomb -> [Coords]
getAllCoords h = [
                        (Coords x y)
                        |
                        y <- [0 .. ((length h) - 1)],
                        x <- [0 .. ((length (h !! y)) - 1)]
                    ]

-- Zwraca listę wszystkich pól w plastrze - zwartości wraz ze współrzędnymi
getAllFields :: Honeycomb -> [FieldWithCoords]
getAllFields h =
    [getField h coords | coords <- (getAllCoords h)]

-- Zwraca nowy plaster, w którym na podanych współrzędnych znajduje się nowa litera
replaceHoneycomb :: Honeycomb -> Coords -> Char -> Honeycomb
replaceHoneycomb h (Coords x y) with = 

    -- Upewnij się, że pole, które zmieniamy jest puste
    assert (
        isNothing (
            (\(FieldWithCoords coords field) -> field)
            (getField h (Coords x y))
        )
    )

    -- Podmień podane pole
    replace2 (Just with) (x, y) h

-- Zwraca listę wszystkich pól przylegających do pola o współrzędnych X,Y
findFieldNeighbours :: Honeycomb -> Coords -> [FieldWithCoords]
findFieldNeighbours h (Coords x y) =
    -- W zależności od tego, czy jesteśmy w szerszym czy węższym wierszu - bierzemy klocki z wyższego i niższego przesunięte o 1 w lewo albo prawo
    let rowLength = (length (h !! y)) in
        if ((rowLength + 1) == (length h)) then
            -- Węższy
            catMaybes [
                -- Wyższy wiersz
                findFieldIfExists h (Coords (x + 0) (y - 1)),
                findFieldIfExists h (Coords (x + 1) (y - 1)),

                -- Ten wiersz
                findFieldIfExists h (Coords (x - 1) (y + 0)), -- Po lewej
                --findFieldIfExists h (Coords (x + 0) (y + 0)), -- Ten
                findFieldIfExists h (Coords (x + 1) (y + 0)), -- Po prawej

                -- Niższy wiersz wiersz
                findFieldIfExists h (Coords (x + 0) (y + 1)),
                findFieldIfExists h (Coords (x + 1) (y + 1))
            ]
        else if ((rowLength == length h)) then
            -- Szerszy
            catMaybes [
                -- Wyższy wiersz
                findFieldIfExists h (Coords (x - 1) (y - 1)),
                findFieldIfExists h (Coords (x + 0) (y - 1)),

                -- Ten wiersz
                findFieldIfExists h (Coords (x - 1) (y + 0)), -- Po lewej
                --findFieldIfExists h (Coords (x + 0) (y + 0)), -- Ten
                findFieldIfExists h (Coords (x + 1) (y + 0)), -- Po prawej

                -- Niższy wiersz wiersz
                findFieldIfExists h (Coords (x - 1) (y + 1)),
                findFieldIfExists h (Coords (x + 0) (y + 1))
            ]
        else
            error "Invalid row length"

-- Zwraca zestaw wszystkich punktów wraz z odpowiadającymi im sąsiadami
getAllFieldsNeighbours :: Honeycomb -> [(FieldWithCoords, [FieldWithCoords])]
getAllFieldsNeighbours h =
    [((FieldWithCoords coords field), findFieldNeighbours h coords) | (FieldWithCoords coords field) <- getAllFields h]

-- Zwraca ile pól spośród podanej listy jest pustych
getNothingCount :: [FieldWithCoords] -> Int
getNothingCount list =
    length (filter isNothing (map (\(FieldWithCoords _ field) -> field) list))

getEmptyPointsSortedByNeighboursFillRatio :: Honeycomb -> [Coords]
getEmptyPointsSortedByNeighboursFillRatio h =
    -- Wytnij tylko współrzędne
    map
    (\
        ((FieldWithCoords coords _), _) ->
            coords
    )
    (
    -- Posortuj według ilości pustych punktów otoczenia
    --sortBy
    --    (\
    --       (_, l1) (_, l2) ->
    --            compare (getNothingCount l2) (getNothingCount l1)
    --    )
        (
            -- Weź tylko puste punkty
            filter
                (\
                    ((FieldWithCoords _ field), _) ->
                        isNothing field
                )
                (getAllFieldsNeighbours h)
        )
    )


-- Sprawdza, czy punkt o podanych współrzędnych zawiera prawidłowe sąsiedztwo - nie ma duplikatów
validateNeighbours :: Honeycomb -> Coords -> Bool
validateNeighbours h coords =
    -- Sprawdź, czy nie ma duplikatów
    isListUnique (
        -- Usuń puste pola
        catMaybes (
            -- Z każdego pola z otoczenia pobierz tylko wartość
            map
                (\(FieldWithCoords coords field) -> field)
                ((findFieldNeighbours h coords) ++ [getField h coords])
        )
    )

validateRow :: Row -> Bool
validateRow row =
	isListUnique (catMaybes row)

validateRows :: [Row] -> Bool
validateRows [] = True
validateRows (x:xs) =
	(validateRow x) && (validateRows xs)

validateAllNeighbours :: Honeycomb -> [Coords] -> Bool
validateAllNeighbours _ [] = True
validateAllNeighbours h (x:xs) =
	(validateNeighbours h x)
	&&
	(validateAllNeighbours h xs)


-- Sprawdza poprawność całego plastra
validateHoneycomb :: Honeycomb -> Bool
validateHoneycomb h = 
    (validateRows h)
    &&
    (validateAllNeighbours h (getAllCoords h))

-- sprawdza czy wszystkie pola w wierszu są uzupełnione
isRowSolved :: Row -> Bool
isRowSolved [] = True
isRowSolved (x:xs) = (isJust x) && isRowSolved xs

-- Sprawdza, czy podany plaster miodu jest ułożony.
-- Nie sprawdza poprawności, jedynie uzupełnienie wszystkich pól
isSolved :: Honeycomb -> Bool
isSolved [] = True
isSolved (x:xs) = (isRowSolved x) && isSolved xs