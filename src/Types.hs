module Types where

type Field = Maybe Char

type Row = [Field]

type HoneyComb = [Row]

data Plaster = Plaster [String] deriving (Read, Show)