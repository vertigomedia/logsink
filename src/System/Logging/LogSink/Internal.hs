{-# LANGUAGE RecordWildCards #-}
module System.Logging.LogSink.Internal where

import           Prelude ()
import           System.Logging.LogSink.Compat

import           Control.Concurrent
import           Data.Char
import           Data.Maybe (fromMaybe)
import           Data.Time.Clock
import           Data.Time.Format (formatTime, defaultTimeLocale)
import           Data.Time.LocalTime ()
import           System.Logging.Facade.Types

type Format = LogRecord -> IO String

defaultFormatString :: String
defaultFormatString = "{level}: {message}"

data Node = Level
          | Message
          | Timestamp
          | ThreadId
          | Literal String
          | LocationPackage
          | LocationModule
          | LocationFile
          | LocationLine
  deriving (Eq, Show)

formatNodes :: [Node] -> LogRecord -> IO String
formatNodes nodes LogRecord{..} = concat <$> mapM evalNode nodes
  where
    evalNode :: Node -> IO String
    evalNode node = case node of
      Level -> return (show logRecordLevel)
      Message -> return logRecordMessage
      Timestamp -> formatISO8601Millis <$> getCurrentTime
      ThreadId -> do
        tid <- myThreadId
        return $ filter isNumber (show tid)
      Literal s -> return s
      LocationPackage -> return $ fromMaybe "" $ locationPackage <$> logRecordLocation
      LocationModule -> return $ fromMaybe "" $ locationModule <$> logRecordLocation
      LocationFile -> return $ fromMaybe "" $ locationFile <$> logRecordLocation
      LocationLine -> return $ fromMaybe "" $ show . locationLine <$> logRecordLocation

formatISO8601Millis :: UTCTime -> String
formatISO8601Millis t =
    let str = formatTime defaultTimeLocale "%FT%T%Q" t in
    -- Padding is needed because `formatTime` with "%Q" does not create trailing zeros
    let strPadded = str ++ if length str == 19 then ".000" else "000" in
    take 23 strPadded ++ "Z"

parseNodes :: String -> Either String [Node]
parseNodes = fmap (filter $ not . isEmpty) . go ""
  where
    isIdChar :: Char -> Bool
    isIdChar c = isAlphaNum c || (c `elem` "-_")

    lookupNode :: String -> Maybe Node
    lookupNode key = lookup key
      [ ("level", Level)
      , ("message", Message)
      , ("timestamp", Timestamp)
      , ("thread-id", ThreadId)
      , ("location-package", LocationPackage)
      , ("location-module", LocationModule)
      , ("location-file", LocationFile)
      , ("location-line", LocationLine)
      ]

    go :: String -> String -> Either String [Node]
    go acc input = case input of
      ""  -> return [lit acc]
      '{':xs | (key,'}':ys) <- span isIdChar xs -> case lookupNode key of
        Nothing -> Left ("invalid format directive " ++ show key)
        Just node -> do
          nodes <- go "" ys
          return (lit acc : node : nodes)
      x:xs -> go (x:acc) xs

    lit :: String -> Node
    lit = Literal . reverse

    isEmpty :: Node -> Bool
    isEmpty (Literal "") = True
    isEmpty _ = False
