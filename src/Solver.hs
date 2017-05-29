module Solver where

import Utils
import Types

import Data.List
import Data.Maybe

import Control.Exception.Base

import Debug.Trace
import UiUtils

solveOne :: Honeycomb -> Maybe Honeycomb
solveOne h = 
	--trace ( honeycombToString h 0 ) (
	if validateHoneycomb h then
		if isSolved h then
			Just h
		else
			solveOneCoordsChars h (getEmptyCoords h) honeycombLetters
	else
		Nothing
	--)

solveOneCoordsChars :: Honeycomb -> Coords -> [Char] -> Maybe Honeycomb
solveOneCoordsChars h c [] = Nothing
solveOneCoordsChars h c (x:xs) = 
	let replacedHoneycomb = replaceHoneycomb h c x in
		case solveOne replacedHoneycomb of
			Just h -> Just h
			Nothing -> solveOneCoordsChars h c xs

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
                findFieldIfExists h (Coords (x + 0) (y + 0)), -- Ten
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
                findFieldIfExists h (Coords (x + 0) (y + 0)), -- Ten
                findFieldIfExists h (Coords (x + 1) (y + 0)), -- Po prawej

                -- Niższy wiersz wiersz
                findFieldIfExists h (Coords (x - 1) (y + 1)),
                findFieldIfExists h (Coords (x + 0) (y + 1))
            ]
        else
            error "Invalid row length"


getEmptyCoords :: Honeycomb -> Coords
getEmptyCoords h =
	case getEmptyCoordsHoneycomb 0 h of
		Nothing -> error "Not found"
		Just x -> x

getEmptyCoordsHoneycomb :: Int -> Honeycomb -> Maybe Coords
getEmptyCoordsHoneycomb _ [] = Nothing
getEmptyCoordsHoneycomb y (h:t) =
	case getEmptyCoordsRow y 0 h of
		Just coords -> Just coords
		Nothing -> getEmptyCoordsHoneycomb (y + 1) t

getEmptyCoordsRow :: Int -> Int -> Row -> Maybe Coords
getEmptyCoordsRow _ _ [] = Nothing
getEmptyCoordsRow y x (h:t) = 
	case h of
		Nothing -> Just (Coords x y)
		Just _ -> getEmptyCoordsRow y (x + 1) t

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
                ((findFieldNeighbours h coords))
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