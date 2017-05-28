module Samples where

import Solver
import UiUtils

-- Z zadania
honeyComb1 = [
        [Just 'B', Just 'D', Nothing, Nothing],
        [Nothing, Just 'G', Just 'A', Nothing, Just 'D'],
        [Nothing, Just 'F', Just 'E', Just 'G'],
        [Just 'A', Just 'B', Just 'D', Just 'C', Just 'F'],
        [Just 'E', Nothing, Nothing, Nothing]
    ]

-- Z zadania, z uzupełnionym jednym rogiem
honeyComb2 = [
        [Just 'B', Just 'D', Nothing, Nothing],
        [Just 'C', Just 'G', Just 'A', Nothing, Just 'D'],
        [Nothing, Just 'F', Just 'E', Just 'G'],
        [Just 'A', Just 'B', Just 'D', Just 'C', Just 'F'],
        [Just 'E', Nothing, Nothing, Nothing]
    ]

-- Z zadania, nieprawidłowy
honeyComb3 = [
        [Just 'B', Just 'D', Nothing, Nothing],
        [Just 'B', Just 'G', Just 'A', Nothing, Just 'D'],
        [Nothing, Just 'F', Just 'E', Just 'G'],
        [Just 'A', Just 'B', Just 'D', Just 'C', Just 'F'],
        [Just 'E', Nothing, Nothing, Nothing]
    ]

honeyComb10 = [
        [Just 'C', Just 'E', Just 'F', Just 'B'],
        [Just 'F', Just 'B', Just 'G', Just 'A', Just 'D'],
        [Just 'A', Just 'D', Just 'C', Just 'E'],
        [Just 'C', Just 'E', Just 'F', Just 'B', Just 'G'],
        [Just 'B', Just 'G', Just 'A', Just 'D']
    ]
honeyComb11 = [
        [Just 'C', Just 'E', Just 'F', Just 'B'],
        [Just 'F', Just 'B', Just 'G', Just 'A', Just 'D'],
        [Just 'A', Just 'D', Just 'C', Just 'E'],
        [Just 'C', Just 'E', Just 'F', Just 'B', Just 'G'],
        [Just 'B', Just 'G', Just 'A', Nothing]
    ]