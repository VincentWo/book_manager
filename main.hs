{-# LANGUAGE OverloadedStrings, DeriveGeneric #-}
import Data.Ix
import Data.Aeson
import GHC.Generics
import qualified Data.ByteString.Lazy as Lazy
import Data.Char (isDigit)
import System.IO
import Control.Monad
import Data.Char

data Year = Year Int
     deriving (Eq, Ord, Generic)
instance FromJSON Year
instance ToJSON Year
instance Show Year where
    show (Year year) = 
        if year < 0 then
            (show . negate $ year) ++ " b.c."
        else
            show year

instance Read Year where
    readsPrec _ input =
        let (year, rest) = span isDigit input in
        if take 5 rest == " b.c." then
            [(Year (negate . read $ year), drop 5 rest)]
        else
            [(Year . read $ year, rest)]


data YearDelta = YearDelta Int
     deriving (Show, Eq, Ord)

delta :: Year -> Year -> YearDelta
(Year a) `delta` (Year b) = YearDelta $ a - b

(^+) :: Year -> YearDelta -> Year
(Year a) ^+ (YearDelta b) = Year $ a + b

data Author = Author {name :: String,
                      birth :: Year,
                      death :: Maybe Year}
     deriving (Eq, Generic)

instance FromJSON Author
instance ToJSON Author

instance Show Author where
    show (Author name birth Nothing)
        = name ++ "(*" ++ show birth ++ ")"
    show (Author name birth (Just death))
        = name ++ "(*" ++ show birth ++ ",†" ++ show death ++ ")"

data Book = Book {title :: String,
                  author :: Author,
                  published :: Year}
     deriving (Eq, Generic)

instance FromJSON Book
instance ToJSON Book

instance Show Book where
    show (Book title author published)
        = title ++ " von " ++ (show author) ++ ", erschienen " ++ show published



günther_grass = Author "Günther Grass" (Year 1927) $ Just $ Year 2015
blech_trommel = Book "Die Blechtrommel" günther_grass $ Year 1959

booksFile :: FilePath
booksFile = "books.json"

readBooksFile :: IO Lazy.ByteString
readBooksFile = Lazy.readFile booksFile

readBooks :: IO (Either String [Book])
readBooks = (eitherDecode <$> readBooksFile)

printBooks :: IO ()
printBooks = do
    books <- readBooks
    case books of 
        Left error -> putStrLn error
        Right books -> mapM_ print books
    return ()

writeBooks :: [Book] -> IO ()
writeBooks books = 
    Lazy.writeFile booksFile (encode books)

readDeath :: IO (Maybe Year)
readDeath = do
    putStrLn "Lebt der Autor noch? (J/N)"
    answer <- getLine
    case answer of
        "J" -> return Nothing
        "N" -> do
            putStrLn "In welchem Jahr ist er gestorben?"
            death <- readLn :: IO Year
            return $ Just death
        _ -> readDeath

addBook :: IO ()
addBook = do
    putStrLn "Wie lautet der Titel des Buches?"
    title <- getLine
    putStrLn "In welchem Jahr wurde das Buch veröffentlicht?"
    published <- readLn :: IO Year
    putStrLn "Wie lautet der Name des Autors?"
    name <- getLine
    putStrLn "In welchem Jahr wurde der Autor geboren?"
    birth <- readLn :: IO Year
    death <- readDeath

    let author = Author name birth death
    let book = Book title author published

    current_books <- readBooks
    case current_books of
        Left error -> putStrLn error
        Right books -> writeBooks (book : books)
    putStrLn "Buch wurde erfolgreich hinzugefügt"

main :: IO ()
main = do
    putStrLn "Willkommen im Buch-Manager"
    putStrLn "Was willst du tun?"
    putStrLn "(1) Alle Bücher ausgeben"
    putStrLn "(2) Ein Buch hinzufügen"
    putStrLn "(3) Beenden"
    putStr "( )\b\b"
    hFlush stdout
    choice <- readLn :: IO Int
    case choice of
        1 -> do
            putStrLn ""
            printBooks
            putStrLn ""
            main
        2 -> do addBook; main
        3 -> return ()
        _ -> main


