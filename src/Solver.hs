module Solver where

import Utils
import Types

import Data.List
import Data.Maybe

import Control.Exception.Base

solveOne :: HoneyComb -> (Maybe HoneyComb)
solveOne h =
    --trace ((honeyCombToString h) ++ "-------------------------------------------------------------------------------------------------------------------------\n\n") (
    if isSolved h then
        --assert
        --    (validateHoneyComb h)
            Just h
    else
        let allHoneyCombs = [(coords, char, replaceHoneyComb h coords char) | coords <- getEmptyPointsSortedByNeighboursFillRatio h, char <- honeyCombLetters] in
            let validHoneyCombs = [replacedHoneyComb | (coords, char, replacedHoneyComb) <- allHoneyCombs, validateHoneyComb replacedHoneyComb] in
                solveList validHoneyCombs
    --)

solveList :: [HoneyComb] -> (Maybe HoneyComb)
solveList [] = Nothing
solveList (h:tail) =
    case (solveOne h) of
        Just h -> Just h
        Nothing -> solveList tail


-- Zwraca listę wszystkich liter które mogą pojawić się na planszy
honeyCombLetters :: [Char]
honeyCombLetters = ['A' .. 'G']

getField :: HoneyComb -> Coords -> FieldWithCoords
getField h (Coords x y) = 
    (FieldWithCoords (Coords x y) (h !! y !! x))

-- Zwraca pole plastra o podanych współrzędnych X, Y liczonych od lewego górnego rogu.
-- Jeśli pole nie istnieje - zwraca Nothig
findFieldIfExists :: HoneyComb -> Coords -> Maybe FieldWithCoords
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
getAllCoords :: HoneyComb -> [Coords]
getAllCoords h = [
                        (Coords x y)
                        |
                        y <- [0 .. ((length h) - 1)],
                        x <- [0 .. ((length (h !! y)) - 1)]
                    ]

-- Zwraca listę wszystkich pól w plastrze - zwartości wraz ze współrzędnymi
getAllFields :: HoneyComb -> [FieldWithCoords]
getAllFields h =
    [getField h coords | coords <- (getAllCoords h)]

-- Zwraca nowy plaster, w którym na podanych współrzędnych znajduje się nowa litera
replaceHoneyComb :: HoneyComb -> Coords -> Char -> HoneyComb
replaceHoneyComb h (Coords x y) with = 

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
findFieldNeighbours :: HoneyComb -> Coords -> [FieldWithCoords]
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
getAllFieldsNeighbours :: HoneyComb -> [(FieldWithCoords, [FieldWithCoords])]
getAllFieldsNeighbours h =
    [((FieldWithCoords coords field), findFieldNeighbours h coords) | (FieldWithCoords coords field) <- getAllFields h]

-- Zwraca ile pól spośród podanej listy jest pustych
getNothingCount :: [FieldWithCoords] -> Int
getNothingCount list =
    length (filter isNothing (map (\(FieldWithCoords _ field) -> field) list))

getEmptyPointsSortedByNeighboursFillRatio :: HoneyComb -> [Coords]
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
    --            compare (getNothingCount l1) (getNothingCount l2)
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
validateNeighbours :: HoneyComb -> Coords -> Bool
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

-- Sprawdza poprawność całego plastra
validateHoneyComb :: HoneyComb -> Bool
validateHoneyComb h = 
    all (== True) [validateNeighbours h coords | coords <- getAllCoords h]

isSolved :: HoneyComb -> Bool
isSolved h = 
    -- Wszystkie
    all (== True)
    (
        -- Pola są uzupełnione
        map (\ (FieldWithCoords coords field) -> isJust field)

        -- Na liście pól
        (getAllFields h)
    )