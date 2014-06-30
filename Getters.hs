{-# LANGUAGE OverloadedStrings #-}
module Getters where
import Journal.Internal
import Data.Text.Internal
import qualified Data.Vector as V
import qualified Data.Text as T
import Data.Aeson
import Data.Aeson.Lens
import Control.Lens
import Utils (resolve)


getYear :: Note -> Maybe Text
getYear = (fmap (T.take 4)) . getTimestamp

getTimestamp :: Note -> Maybe Text
getTimestamp = (^? key "timestamp" . _String)

getKmdLmt :: Note -> Maybe Text
getKmdLmt = (^? key "_kmd" . key "lmt" . _String)

getText :: Note -> Maybe Text
getText = (^? key "text" . _String)

getHashTags :: Note -> Maybe [Text]
getHashTags note = 
    let maybeVector = note ^? key "entities" . key "hashtags" . _Array 
        maybeList =  fmap V.toList maybeVector
        valToText (String text) = Just text
        valToText _ = Nothing
        textList = resolve . (fmap valToText)
    in fmap textList maybeList

containsTag :: Text -> Note -> Bool
containsTag tag note =
    let hashtags =  getHashTags note
    in case hashtags of
        Nothing -> False
        Just tags -> elem tag tags