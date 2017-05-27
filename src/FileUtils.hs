module FileUtils where
import Types
import Converters

loadHoneycombFromFile :: IO TextHoneycomb
loadHoneycombFromFile = do
                         putStrLn "Podaj nazwe pliku z zagadka:"
                         fileName <- getLine
                         fileText <- readFile fileName
                         let plaster :: TextHoneycomb
                             plaster = read fileText :: TextHoneycomb
                         return plaster


saveHoneycombToFile :: Maybe HoneyComb -> IO ()
saveHoneycombToFile (Just hc) = do
                       let hcText = convertHoneycombToTextRepr hc
                       putStrLn "Podaj nazwe pliku wyjsciowego: "
                       fileName <- getLine
                       let text = show hcText
                       writeFile fileName text