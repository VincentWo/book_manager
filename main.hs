{-# LANGUAGE OverloadedStrings, DeriveGeneric #-}

import Data.Ix
import Data.Aeson
import Data.List
import Data.Maybe
import GHC.Generics
import qualified Data.ByteString.Lazy as Lazy
import System.IO
import Control.Monad
import Data.Char
import System.Environment
import Safe
import Text.Read

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
        case readMaybe year of
            Just year ->
                if " b.c." `isPrefixOf` rest then
                    [(Year $ negate year, drop 5 rest)]
                else
                    [(Year year, rest)]
            Nothing   -> []
        where (year, rest) = span isDigit input


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

helpString :: IO String
helpString = do
    progName <- getProgName
    return $ "Usage: " ++ progName ++ " action [arguments...]\n"

booksFile :: FilePath
booksFile = "books.json"

readBooksFile :: IO Lazy.ByteString
readBooksFile = Lazy.readFile booksFile

readBooks :: IO (Either String [Book])
readBooks = (eitherDecode <$> readBooksFile)

split :: Eq a => a -> [a] -> ([a], [a])
split x (y:ys)
    | x == y    = ([], ys)
    | otherwise = (y : ys1, ys2)
    where (ys1, ys2) = split x ys

split _ [] = ([], [])

getOptions :: [String] -> [(String, String)]
getOptions = map $ dropFst . split '='
    where dropFst (x,y) = (drop 2 x, y)

getFilter :: [String] -> (String, String, Maybe Year)
getFilter arguments =
    (fromMaybe "" $ getArgument "author",
     fromMaybe "" $ getArgument "title",
     getArgument "published" >>= readMaybe :: Maybe Year)
    where
        getArgument
            = liftM fst . lastMay . filteredOptions
        filteredOptions flag
            = filter ((== flag) . fst) $ getOptions arguments


printBooks :: [String] -> IO ()
printBooks arguments = do
    let (authorFilter, titleFilter, publishFilter) = getFilter arguments
    books <- readBooks

    case books of 
        Left error
            -> putStrLn error
        Right books
            -> mapM_ print $ filter predicate books
                where predicate book
                        = authorFilter `isSubsequenceOf` authorName
                          &&
                          titleFilter `isSubsequenceOf` bookTitle
                          &&
                          publishFilter == Nothing
                          ||
                          publishFilter == Just publishDate
                            where authorName = map toLower $ name $ author book
                                  bookTitle = map toLower $ title book
                                  publishDate = published book
    return ()

writeBooks :: [Book] -> IO ()
writeBooks books = 
    Lazy.writeFile booksFile $ encode $ sort books

addBook :: [String] -> IO ()
addBook (title:published:name:birth:arguments) = do
    let death = listToMaybe arguments
    let author = Author name (read birth :: Year) (read <$> death :: Maybe Year)
    let book = Book title author $ read published

    current_books <- readBooks
    case current_books of
        Left error  -> do putStrLn error
        Right books -> do writeBooks (book : books)
                          putStrLn "Buch wurde erfolgreich hinzugefügt"

addBook _ = do
    putStrLn "Nicht genügend Argumente"
    helpString >>= putStr

unknownAction :: String -> IO ()
unknownAction action = do
    putStrLn $ "Unbekannte Aktion: " ++ action
    helpString >>= putStr

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
            -> do
                putStrLn "Bitte gib eine Aktion an."
                helpString >>= putStr

