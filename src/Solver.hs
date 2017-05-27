module Solver where

import Types
import Data.Maybe

-- Zwraca listę wszystkich współrzędnych punktów w plastrze
findAllCoords :: HoneyComb -> [Coords]
findAllCoords h = [
                        (Coord x y)
                        |
                        y <- [0 .. ((length h) - 1)],
                        x <- [0 .. ((length (h !! y)) - 1)]
                    ]

-- Zwraca pole plastra o podanych współrzędnych X, Y liczonych od lewego górnego rogu.
-- Jeśli pole nie istnieje - zwraca Nothig
findFieldIfExists :: HoneyComb -> Coords -> Maybe Field
findFieldIfExists h (Coord x y) =
    if y >= 0 && y < length h then
        let row = h !! y in
        if x >= 0 && x < length row then
            let field = row !! x in
                Just field
        else
            Nothing
    else
        Nothing

-- Zwraca listę wszystkich pól przylegających do pola o współrzędnych X,Y wraz z tym polem
findFieldNeighbours :: HoneyComb -> Coords -> [Field]
findFieldNeighbours h (Coord x y) =
    -- W zależności od tego, czy jesteśmy w szerszym czy węższym wierszu - bierzemy klocki z wyższego i niższego przesunięte o 1 w lewo albo prawo
    if even x then
        -- Węższy
        catMaybes [
            -- Wyższy wiersz
            findFieldIfExists h (Coord (x + 0) (y - 1)),
            findFieldIfExists h (Coord (x + 1) (y - 1)),

            -- Ten wiersz
            findFieldIfExists h (Coord (x + 0) (y + 0)), -- Ten
            findFieldIfExists h (Coord (x - 1) (y + 0)), -- Po lewej
            findFieldIfExists h (Coord (x + 1) (y + 0)), -- Po prawej

            -- Niższy wiersz wiersz
            findFieldIfExists h (Coord (x + 0) (y + 1)),
            findFieldIfExists h (Coord (x + 1) (y + 1))
        ]
    else
        -- Szerszy
        catMaybes [
            -- Wyższy wiersz
            findFieldIfExists h (Coord (x - 1) (y - 1)),
            findFieldIfExists h (Coord (x + 0) (y - 1)),

            -- Ten wiersz
            findFieldIfExists h (Coord (x + 0) (y + 0)), -- Ten
            findFieldIfExists h (Coord (x - 1) (y + 0)), -- Po lewej
            findFieldIfExists h (Coord (x + 1) (y + 0)), -- Po prawej

            -- Niższy wiersz wiersz
            findFieldIfExists h (Coord (x - 1) (y + 1)),
            findFieldIfExists h (Coord (x + 0) (y + 1))
        ]

-- Zwraca pozycję (środka) wokół której plaster miodu jest najbardziej uzupełniony
--findAlmostFilledPlace :: [Coords] -> Maybe Coord
    -- 
