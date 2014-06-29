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
import Control.Monad
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
    let maybeVector = note ^? key "entities" . key "hashtags" . _Array 
        maybeList =  fmap V.toList maybeVector
        valToText (String text) = Just text
        valToText _ = Nothing
        textList = resolve . (fmap valToText)
    in fmap textList maybeList

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

month = (T.take 3) . (T.tail)

isTagDate :: Text -> Bool
isTagDate = (`elem` months) . month

enumTagDate :: Text -> Text
enumTagDate tagDate =
    let m = month tagDate
        day = T.drop 4 tagDate
        hundreds = case elemIndex m months of
            Nothing -> 0
            Just index -> index + 1
    in T.concat [(T.pack . show) hundreds, day]

safeHead :: [a] -> Maybe a
safeHead list =
    if length list > 0
    then Just (head list)
    else Nothing

getTagDate :: Note -> Maybe Text
getTagDate = (>>= safeHead) . (fmap (filter isTagDate)) . getHashTags

compareMaybes :: (Ord b) => (a -> Maybe b) -> a -> a -> Ordering
compareMaybes getter item1 item2 = 
    let prop1 = getter item1
        prop2 = getter item2
    in case (prop1, prop2) of 
        (Just thing1, Just thing2) -> compare thing1 thing2
        (Just thing, Nothing) -> GT
        _ -> EQ

equalYears :: Note -> Note -> Bool
equalYears note1 note2 = EQ == (compareMaybes getYear note1 note2)

compareYears :: Note -> Note -> Ordering
compareYears = compareMaybes getYear

compareTagDates :: Note -> Note -> Ordering
compareTagDates = compareMaybes getTagDate

--displayYear :: [Note] -> IO ()
--displayYear notes =
--    sequence_ $ fmap displayNote (sortBy (\x y ->  ))

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
            print $  fmap (resolve . (fmap getTagDate)) byYear
        _ ->  print "yo"

