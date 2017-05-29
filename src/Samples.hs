module Samples where

import Solver

-- Z zadania
honeycomb1 = [
        [Just 'B', Just 'D', Nothing, Nothing],
        [Nothing, Just 'G', Just 'A', Nothing, Just 'D'],
        [Nothing, Just 'F', Just 'E', Just 'G'],
        [Just 'A', Just 'B', Just 'D', Just 'C', Just 'F'],
        [Just 'E', Nothing, Nothing, Nothing]
    ]

-- Z zadania, z uzupełnionym jednym rogiem
honeycomb2 = [
        [Just 'B', Just 'D', Nothing, Nothing],
        [Just 'C', Just 'G', Just 'A', Nothing, Just 'D'],
        [Nothing, Just 'F', Just 'E', Just 'G'],
        [Just 'A', Just 'B', Just 'D', Just 'C', Just 'F'],
        [Just 'E', Nothing, Nothing, Nothing]
    ]

-- Z zadania, nieprawidłowy
honeycomb3 = [
        [Just 'B', Just 'D', Nothing, Nothing],
        [Just 'B', Just 'G', Just 'A', Nothing, Just 'D'],
        [Nothing, Just 'F', Just 'E', Just 'G'],
        [Just 'A', Just 'B', Just 'D', Just 'C', Just 'F'],
        [Just 'E', Nothing, Nothing, Nothing]
    ]

honeycomb10 = [
        [Just 'C', Just 'E', Just 'C', Just 'B'],
        [Just 'F', Just 'B', Just 'G', Just 'A', Just 'D'],
        [Just 'A', Just 'D', Just 'C', Just 'E'],
        [Just 'C', Just 'E', Just 'F', Just 'B', Just 'G'],
        [Just 'B', Just 'G', Just 'A', Just 'D']
    ]
honeycomb11 = [
        [Just 'C', Just 'E', Just 'F', Just 'B'],
        [Just 'F', Just 'B', Just 'G', Just 'A', Just 'D'],
        [Just 'A', Just 'D', Just 'C', Just 'E'],
        [Just 'C', Just 'E', Just 'F', Just 'B', Just 'G'],
        [Just 'B', Just 'G', Just 'A', Nothing]
    ]

honeycomb12 = [
        [Nothing, Nothing, Nothing, Nothing],
        [Nothing, Nothing, Nothing, Nothing, Nothing],
        [Nothing, Nothing, Nothing, Nothing],
        [Nothing, Nothing, Nothing, Nothing, Nothing],
        [Nothing, Nothing, Nothing, Nothing]
    ]

honeycomb13 = [
        [Nothing, Nothing],
        [Nothing, Nothing, Nothing],
        [Nothing, Nothing]
    ]
honeycomb14 = [
        [Nothing, Nothing, Nothing],
        [Nothing, Nothing, Nothing, Nothing],
        [Nothing, Nothing, Nothing],
        [Nothing, Nothing, Nothing, Nothing]
    ]


honeycomb15 = [
        [Nothing, Nothing, Nothing, Nothing, Nothing],
        [Nothing, Nothing, Nothing, Nothing, Nothing, Nothing],
        [Nothing, Nothing, Nothing, Nothing, Nothing],
        [Nothing, Nothing, Nothing, Nothing, Nothing, Nothing],
        [Nothing, Nothing, Nothing, Nothing, Nothing],
        [Nothing, Nothing, Nothing, Nothing, Nothing, Nothing]
    ]
-- duzy przyklad
honeycomb4 = [
	    [Nothing, Nothing, Nothing, Nothing, Nothing, Nothing],
	    [Nothing, Nothing, Nothing, Nothing, Nothing, Nothing],
	    [Nothing, Nothing, Just 'C', Just 'B', Just 'A', Just 'D'],
	    [Nothing, Nothing, Nothing, Nothing, Just 'G', Nothing, Nothing],
	    [Nothing, Nothing, Nothing, Nothing, Nothing, Nothing],
	    [Nothing, Nothing, Nothing, Nothing, Nothing, Nothing, Nothing],
	    [Nothing, Nothing, Just 'E', Nothing, Nothing, Nothing]
	]