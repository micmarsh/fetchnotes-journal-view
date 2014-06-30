{-# LANGUAGE OverloadedStrings #-}
module Sorting where
import Data.Text.Internal

import Data.List
import Getters
import Utils
import Data.Aeson
import qualified Data.Text as T
import Type



months :: [Text]
months = ["jan", "feb", "mar", "apr", "may", "jun", "jul", "aug", "sep", "oct", "nov", "dec"]

month = (T.take 3) . T.tail
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
compareTagDates = compareMaybes ((fmap enumTagDate) . getTagDate)
