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
import qualified Data.Map as Map
import Data.Tuple.Lazy

data Year = Year Int
     deriving (Eq, Ord, Generic)
instance FromJSON Year
instance ToJSON Year

instance Show Year where
    show (Year year) = 
        if year < 0 then
            (++ " b.c.") . show . negate $ year
        else
            show year

instance Read Year where
    readsPrec _ input =
        case readMaybe year of
            Just year ->
                if " b.c." `isPrefixOf` rest then
                    [(Year . negate $ year, drop 5 rest)]
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

(^-) :: Year -> YearDelta -> Year
(Year a) ^- (YearDelta b) = Year $ a - b

data Author = Author {name :: String,
                      birth :: Year,
                      death :: Maybe Year} deriving (Eq, Generic)

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


defaultFile :: FilePath
defaultFile = "books.json"

getFilename :: Map.Map String String -> FilePath
getFilename = maybe defaultFile id . Map.lookup "data"

readBooks :: Map.Map String String -> IO (Either String [Book])
readBooks arguments = 
        eitherDecode <$> file
            where file = Lazy.readFile $ getFilename arguments


printBooks :: [String] -> Map.Map String String -> IO ()
printBooks _ arguments = do
    let authorFilter
            = liftM (map toLower) $ Map.lookup "author" arguments
        titleFilter
            = Map.lookup "title" arguments
        publishFilter
            = Map.lookup "published" arguments >>= readMaybe :: Maybe Year
    books <- readBooks arguments

    case books of 
        Left error
            -> putStrLn error
        Right books
            -> mapM_ print $ filter predicate books
                where
                    predicate book
                        = maybePredicate
                            authorFilter (`isSubsequenceOf` authorName)
                          &&
                          maybePredicate
                            titleFilter (`isSubsequenceOf` bookTitle)
                          &&
                          maybePredicate
                            publishFilter (== publishDate)
                        where
                            authorName = map toLower $ name $ author book
                            bookTitle = map toLower $ title book
                            publishDate = published book
                            maybePredicate (Just toTest) p = p toTest
                            maybePredicate Nothing _ = True
    return ()

writeBooks :: [Book] -> Map.Map String String -> IO ()
writeBooks books arguments 
    = Lazy.writeFile fileName $ encode $ sort books
        where
            fileName = getFilename arguments

addBook :: [String] -> Map.Map String String -> IO ()
addBook (title:published:name:birth:rest) arguments = do

    let author = Author name (read birth :: Year) (liftM read $ headMay rest :: Maybe Year)
    let book = Book title author $ read published

    current_books <- readBooks arguments
    case current_books of
        Left error  -> do putStrLn error
        Right books -> do writeBooks (book : books) arguments
                          putStrLn "Buch wurde erfolgreich hinzugefügt"
addBook _ _ = do
    putStrLn "Nicht genügend Argumente"
    helpString >>= putStr

unknownAction :: String -> IO ()
unknownAction action = do
    putStrLn $ "Unbekannte Aktion: " ++ action
    helpString >>= putStr

dispatch :: [String] -> Map.Map String String -> IO ()
dispatch ("view" : actions) = printBooks actions
dispatch ("add"  : actions) = addBook actions
dispatch (action : _)       = const (unknownAction action)


split :: Eq a => a -> [a] -> ([a], [a])
split x (y:ys)
    | x == y    = ([], ys)
    | otherwise = (y : ys1, ys2)
    where (ys1, ys2) = split x ys
split _ [] = ([], [])

parseParameters :: [String] -> ([String], Map.Map String String)
parseParameters parameters = (actions, arguments)
    where
        arguments = Map.fromList $ map (split '=' . drop 2) argumentList
        (argumentList, actions) = partition ("--" `isPrefixOf`) parameters 

main :: IO ()
main = do
    parameters <- getArgs
    let (actions, arguments) = parseParameters parameters
    case actions of
        []
            -> putStrLn "Bitte gib eine Aktion an."
        actions
            -> dispatch actions arguments 
