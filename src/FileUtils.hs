module FileUtils where
import Types
import Converters

loadHoneyCombFromFile :: IO TextHoneyComb
loadHoneyCombFromFile = do
                         putStrLn "Podaj nazwe pliku z zagadka:"
                         fileName <- getLine
                         fileText <- readFile fileName
                         let plaster :: TextHoneyComb
                             plaster = read fileText :: TextHoneyComb
                         return plaster


saveHoneyCombToFile :: Maybe HoneyComb -> IO ()
saveHoneyCombToFile (Just hc) = do
                       let hcText = convertHoneyCombToTextRepr hc
                       putStrLn "Podaj nazwe pliku wyjsciowego: "
                       fileName <- getLine
                       let text = show hcText
                       writeFile fileName text