module FileUtils where
import Types


loadPlasterFromFile :: IO Plaster
loadPlasterFromFile = do
                        putStrLn "Podaj nazwe pliku z zagadka:"
                        fileName <- getLine
                        fileText <- readFile fileName
                        let plaster :: Plaster
                            plaster = read fileText :: Plaster
                        return plaster