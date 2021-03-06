module Types where

-- Typ opisujący współrzędne pola.
-- Kolejno X, Y, liczone od lewego górnego rogu.
-- Plaster jest zorganizowny tak, jakby elementy w poszczególnych wierszach były wyrównane do lewej krawędzi
data Coords = Coords Int Int deriving (Show)

-- Typ opisujący pole plastra
-- Może być albo puste - Nothig, albo zawierać literę - Just
type Field = Maybe Char

-- Pole plastra wraz ze współrzędnymi
data FieldWithCoords = FieldWithCoords Coords Field deriving (Show)

-- Typ opisujący wiersz plastra
type Row = [Field]

-- Typ opisujący cały plaster
type Honeycomb = [Row]

-- Typ opisujący strukturę plastra odpowiadającą strukturze pliku wejściowego
data TextHoneycomb = Plaster [String] deriving (Read, Show)