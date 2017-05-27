module Types where

data Coords = Coord Int Int

type Field = Maybe Char

type Row = [Field]

type HoneyComb = [Row]

data Plaster = Plaster [String] deriving (Read, Show)