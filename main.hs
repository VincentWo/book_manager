{-# LANGUAGE OverloadedStrings, DeriveGeneric #-}

import Data.Ix
import Data.Aeson
import Data.List
import Data.Maybe
import qualified Data.Text.ICU as ICU
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
import System.IO.Error

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

data BookStatus = ToRead | Read | Reading
    deriving (Eq, Generic, Show)

instance FromJSON BookStatus
instance ToJSON BookStatus


data Book = Book {title     :: String,
                  author    :: Author,
                  published :: Year,
                  status    :: BookStatus}
     deriving (Eq, Generic)

instance FromJSON Book
instance ToJSON Book

instance Show Book where
    show (Book title author published status)
        = '>' : title ++ "<"
          ++ " von " ++ (show author)
          ++ ", erschienen " ++ show published
          ++ if status == Read         then " ✓"
             else if status == Reading then " ◁"
             else                           ""
instance Ord Book where
    (Book titleA _ _ _) `compare` (Book titleB _ _ _)
        = ICU.collateIter
            collator (ICU.fromString titleA) (ICU.fromString titleB)
        where collator = ICU.collator ICU.Current

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
        eitherDecode <$> catchIOError content handler
            where content = Lazy.readFile $ getFilename arguments
                  handler catched
                    | isDoesNotExistError catched = return "[]"
                    | otherwise                   = ioError catched


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
            -> mapM_ prettyPrint $ filter (predicate . snd) $ zip [1..] books
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
                    prettyPrint (id, book)
                        = putStrLn $ show id ++ ": " ++ show book
    return ()

writeBooks :: [Book] -> Map.Map String String -> IO ()
writeBooks books arguments 
    = Lazy.writeFile fileName $ encode $ sort books
        where
            fileName = getFilename arguments

addBook :: [String] -> Map.Map String String -> IO ()
addBook (title:published:name:birth:rest) arguments = do
    let author = Author name (read birth :: Year) (liftM read $ headMay rest :: Maybe Year)

    let status = if Map.lookup "unread" arguments == Nothing
                 then Read
                 else ToRead

    let book = Book title author (read published) status

    current_books <- readBooks arguments
    case current_books of
        Left error  -> do putStrLn error
        Right books -> do writeBooks (book : books) arguments
                          putStrLn "Buch wurde erfolgreich hinzugefügt"
addBook _ _ = do
    putStrLn "Nicht genügend Argumente"
    helpString >>= putStr

removeNth :: Integral a => a -> [b] -> [b]
removeNth _ [] = []
removeNth 0 (x:xs) = xs
removeNth n (x:xs) = x : removeNth (n - 1) xs

removeBook :: [String] -> Map.Map String String -> IO ()
removeBook [toRemove] arguments = do
    let idToRemove = (subtract 1) . read $ toRemove :: Int

    currentBooks <- readBooks arguments
    case currentBooks of
        Left  error -> do putStrLn error
        Right books -> do writeBooks newBooks arguments
            where newBooks = removeNth idToRemove books

removeBook _ _ = do
    putStrLn "Errors: Too many arguments"
    putStrLn "You can only delete one Book at a time"

{-
editBook toEdit arguments = do
    let idToEdit = (subtract 1) . read $ toEdit :: Int

    currentBooks <- readBooks arguments
    case currentBooks of
        Left  error -> putStrLn error
        Right books -> 
-}
    


unknownAction :: String -> IO ()
unknownAction action = do
    putStrLn $ "Unbekannte Aktion: " ++ action
    helpString >>= putStr

dispatch :: [String] -> Map.Map String String -> IO ()
dispatch ("view"   : actions) = printBooks actions
dispatch ("add"    : actions) = addBook actions
dispatch ("remove" : actions) = removeBook actions
--dispatch ("edit"   : actions) = editBook actions
dispatch (action   : _)       = const (unknownAction action)


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
    (actions, arguments) <- liftM parseParameters getArgs
    case actions of
        []      -> putStrLn "Bitte gib eine Aktion an."
        actions -> dispatch actions arguments 
