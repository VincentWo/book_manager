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
import Safe
import Text.Read
import qualified Data.Map as Map
import Data.Tuple.Lazy

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

günther_grass = Author "Günther Grass" (Year 1927) $ Just $ Year 2015
blech_trommel = Book "Die Blechtrommel" günther_grass $ Year 1959

booksFile :: FilePath
booksFile = "books.json"

readBooksFile :: IO Lazy.ByteString
readBooksFile = Lazy.readFile booksFile

readBooks :: IO (Either String [Book])
readBooks = (eitherDecode <$> readBooksFile)

getFilter :: [String] -> (String, String, Maybe Year)
getFilter arguments = (getArgument "author", getArgument "title", readMaybe $ getArgument "published" :: Maybe Year)
    where getArgument argument
            = drop (length argument + 3) $ getArgumentUnstripped argument
          getArgumentUnstripped argument
            = map toLower $ headDef "" $ filteredArguments argument
          filteredArguments argument
            = filter (("--" ++ argument ++ "=") `isPrefixOf`) arguments

printBooks :: [String] -> Map.Map String String -> IO ()
printBooks _ arguments = do
    let authorFilter  = fmap toLower $ Map.lookup "author" arguments
        titleFilter   = Map.lookup "title" arguments
        publishFilter = readMaybe $ Map.lookup "published" arguments :: Maybe Year
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

addBook :: [String] -> Map.Map String String -> IO ()
addBook (title:published:name:birth:death:_) _ = do

    let author = Author name (read birth :: Year) (Just $ read death :: Maybe Year)
    let book = Book title author $ read published

    current_books <- readBooks
    case current_books of
        Left error  -> do putStrLn error
        Right books -> do writeBooks (book : books)
                          putStrLn "Buch wurde erfolgreich hinzugefügt"
addBook _ _ = do
    putStrLn "Nicht genügend Argumente"

unknownAction :: String -> IO ()
unknownAction action =
    putStrLn $ "Unbekannte Aktion: " ++ action

dispatch :: [String] -> Map.Map String String -> IO ()
dispatch ("view" : actions) = printBooks actions
dispatch ("add"  : actions) = addBook actions
dispatch (action : _)       = const (unknownAction action)


parseParameters :: [String] -> ([String], Map.Map String String)
parseParameters parameters = (actions, arguments)
    where
        arguments = Map.fromList $ map (mapSnd tail . break (== '=')) argumentsList
        (actions, argumentsList) = partition ("--" `isPrefixOf`) parameters 
main :: IO ()
main = do
    parameters <- getArgs
    let (actions, arguments) = parseParameters parameters
    case actions of
        []
            -> putStrLn "Bitte gib eine Aktion an."
        actions
            -> dispatch actions arguments 
