{-# LANGUAGE OverloadedStrings #-}
module Main where
-- string stuff
import Data.ByteString.Lazy.Internal
import Data.Text.Internal
-- real stuff
import qualified Data.ByteString.Lazy as BL
import System.Environment
import qualified Data.Text as T
import Control.Monad
import Data.Aeson
import Data.List
import Type


import Sorting
import Getters
import Utils


readNotes :: String -> IO (Maybe [Note])
readNotes file = fmap decode $ BL.readFile file


checkSameness :: String -> (Note -> Maybe Text) -> [Note] -> IO ()
checkSameness string function notes = do
    let things = (group . (fmap (T.take 10)) . resolve) $ fmap function notes
        multiples = filter (> 1) $ fmap length things
        max' = foldl' max 0 
    putStrLn ("Sets of the same " ++ string)
    print multiples
    putStrLn $ "The most with the same: " ++ (show (max' multiples)) ++ "\n"


displayYear :: [Note] -> IO ()
displayYear notes =
    let sorted = sortBy compareTagDates notes
    in sequence_ $ fmap displayNote sorted
    where 
        parseMaybe (Just text) = text
        parseMaybe Nothing = ""
        displayNote = putStrLn . T.unpack . (`T.append` "\n") . parseMaybe . getText

main :: IO ()
main = do
    args <- getArgs
    (Just notes) <- (readNotes "notes.json")
    case args of
        [ ] -> putStrLn "You need to provide some arguments"
        ("check" : _) -> do
            checkSameness "timestamp" getTimestamp notes
            checkSameness "_kmd.lmt" getKmdLmt notes
        ("view": _) -> do
            let journal = filter (containsTag "#journal") notes
                grouped = groupBy equalYears journal
                byYear = sortBy (\x y -> compareYears (head x) (head y)) $ grouped
            sequence_ $ fmap displayYear byYear
        _ ->  print "yo"

