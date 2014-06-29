{-# LANGUAGE OverloadedStrings #-}
module Main where
-- string stuff
import Data.ByteString.Lazy.Internal
import Data.Text.Internal
-- real stuff
import qualified Data.ByteString.Lazy as BL
import qualified Data.HashMap.Strict as H
import System.Environment
import Data.List
import qualified Data.Text as T
import qualified Data.Vector as V
import Data.Aeson
import Data.Aeson.Lens
import Control.Lens

type Note = Value

readFile' = BL.readFile

readNotes :: String -> IO (Maybe [Note])
readNotes file = fmap decode $ readFile' file

getTimestamp :: Note -> Maybe Text
getTimestamp = (^? key "timestamp" . _String)

getKmdLmt :: Note -> Maybe Text
getKmdLmt = (^? key "_kmd" . key "lmt" . _String)

getHashTags :: Note -> Maybe [Text]
getHashTags note = 
    let vector = note ^? key "entities" . key "hashtags" . _Array -- Maybe (Vector Value)
        list =  fmap V.toList vector
        valToText (String text) = Just text
        valToText _ = Nothing
        textList = resolve . (fmap valToText)
    in fmap textList list
        -- TODO this^ is a  rn, make it a Maybe [Text]

resolve :: [Maybe a] -> [a]
resolve maybes = resolve' [] maybes
    where
        resolve' acc [] = reverse acc
        resolve' acc ((Just thing) : rest) = resolve' (thing : acc) rest
        resolve' acc (Nothing : rest) = resolve' acc rest

checkSameness :: String -> (Note -> Maybe Text) -> [Note] -> IO ()
checkSameness string function notes = do
    let things = (group . resolve) $ fmap function notes
    putStrLn ("Sets of the same " ++ string)
    print $ filter (> 1) $ fmap length things

containsTag :: Text -> Note -> Bool
containsTag tag note =
    let hashtags =  getHashTags note
    in case hashtags of
        Nothing -> False
        Just tags -> elem tag tags

getYear :: Note -> Maybe Text
getYear = (fmap (T.take 4)) . getTimestamp

months :: [Text]
months = ["jan", "feb", "mar", "apr", "may", "jun", "jul", "aug", "sep", "oct", "nov", "dec"]

isTagDate :: Text -> Bool
isTagDate = (`elem` months) . (T.take 3) . (T.tail)

getTagDate :: Note -> Maybe Text
getTagDate note = 
    let hashtags = getHashTags note
        -- TODO replace this filterer with something that selects only datetags
        justdate = fmap (filter isTagDate) hashtags
    in justdate >>= \list -> 
            if length list > 0
            then Just (head list)
            else Nothing

equalYears :: Note -> Note -> Bool
equalYears note1 note2 =
    let year1 = getYear note1
        year2 = getYear note2
    in case (year1, year2) of 
        (Just y1, Just y2) -> y1 == y2
        _ -> False

compareYears :: Note -> Note -> Ordering
compareYears note1 note2 = 
    let year1 = getYear note1
        year2 = getYear note2
    in case (year1, year2) of 
        (Just y1, Just y2) -> compare y1 y2
        (Just y, Nothing) -> GT
        _ -> LT 

main ::IO ()
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
                byYear = sortBy (\x y -> compareYears (head x) (head y)) grouped
            print $ fmap (fmap getTagDate) byYear
        _ ->  print "yo"

