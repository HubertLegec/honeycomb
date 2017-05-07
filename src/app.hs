import System.IO

data Plaster = Plaster [String] deriving (Read, Show)


main = do
        putStrLn "Podaj nazwe pliku z zagadka:"
        fileName <- getLine
        fileText <- readFile fileName
        let honeyComb :: Plaster
            honeyComb = read fileText :: Plaster
        putStrLn (show honeyComb)
