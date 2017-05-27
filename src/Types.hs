module Types where

-- Typ opisujący współrzędne pola.
-- Kolejno X, Y, liczone od lewego górnego rogu.
-- Plaster jest zorganizowny tak, jakby elementy w poszczególnych wierszach były wyrównane do lewej krawędzi
data Coords = Coord Int Int deriving (Show)

-- Typ opisujący pole plastra
-- Może być albo puste - Nothig, albo zawierać literę - Just
type Field = Maybe Char

-- Typ opisujący wiersz plastra
type Row = [Field]

-- Typ opisujący cały plaster
type HoneyComb = [Row]

data Plaster = Plaster [String] deriving (Read, Show)