{-# LANGUAGE OverloadedStrings, DeriveGeneric #-}

import Data.Ix
import Data.Aeson
import Data.List
import Data.Maybe
import GHC.Generics
import qualified Data.ByteString.Lazy as Lazy
import Data.Char (isDigit)
import System.IO
import Control.Monad
import Data.Char
import System.Environment

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
        = name ++ "(*" ++ show birth ++ ", †" ++ show death ++ ")"

data Book = Book {title :: String,
                  author :: Author,
                  published :: Year}
     deriving (Eq, Generic)

instance FromJSON Book
instance ToJSON Book

instance Show Book where
    show (Book title author published)
        = title ++ " von " ++ (show author) ++ ", erschienen " ++ show published
instance Ord Book where
    (Book titleA _ _) `compare` (Book titleB _ _) = titleA `compare` titleB

günther_grass = Author "Günther Grass" (Year 1927) $ Just $ Year 2015
blech_trommel = Book "Die Blechtrommel" günther_grass $ Year 1959

booksFile :: FilePath
booksFile = "books.json"

readBooksFile :: IO Lazy.ByteString
readBooksFile = Lazy.readFile booksFile

readBooks :: IO (Either String [Book])
readBooks = (eitherDecode <$> readBooksFile)

printBooks :: [String] -> IO ()
printBooks arguments = do
    
    let author_filter
            = map toLower $ fromMaybe "" $ listToMaybe author_predicates
                where author_predicates
                        = filter ("--author=" `isPrefixOf`) arguments
    let title_filter
            = map toLower $ fromMaybe "" $ listToMaybe title_predicates
                where title_predicates
                        = filter ("--title=" `isPrefixOf`) arguments
    books <- readBooks

    case books of 
        Left error
            -> putStrLn error
        Right books
            -> mapM_ print $ filter predicate books
                where predicate book
                        = (drop 9 author_filter) `isSubsequenceOf` author_name
                          &&
                          (drop 8 title_filter) `isSubsequenceOf` book_title
                            where author_name = map toLower $ name $ author book
                                  book_title = map toLower $ title book
    return ()

writeBooks :: [Book] -> IO ()
writeBooks books = 
    Lazy.writeFile booksFile $ encode $ sort books

addBook :: [String] -> IO ()
addBook (title:published:name:birth:death:_) = do

    let author = Author name (read birth :: Year) (Just $ read death :: Maybe Year)
    let book = Book title author $ read published

    current_books <- readBooks
    case current_books of
        Left error  -> do putStrLn error
        Right books -> do writeBooks (book : books)
                          putStrLn "Buch wurde erfolgreich hinzugefügt"
addBook _ = do
    putStrLn "Nicht genügend Argumente"

unknownAction :: String -> IO ()
unknownAction action =
    putStrLn $ "Unbekannte Aktion: " ++ action

dispatch :: String -> [String] -> IO ()
dispatch "view" = printBooks
dispatch "add"  = addBook
dispatch action = const (unknownAction action)

main :: IO ()
main = do
    arguments <- getArgs
    case arguments of
        (action:parameters)
            -> dispatch action parameters
        []                 
            -> putStrLn "Bitte gib eine Aktion an."
