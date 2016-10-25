{-# LANGUAGE OverloadedStrings #-}
module Slack.Export.Opts
    ( opts
    , SlackOpts(..)
    ) where

import Control.Monad                (filterM)
import Control.Monad.IO.Class       (liftIO)
import Control.Monad.Trans.Class    (lift)

import Data.Text                    (Text)

import Options.Applicative
import Options.Applicative.Text     (text)

import System.Directory             (doesFileExist)

data SlackOpts = SlackOpts
    { esUrl         :: Text
    , bulkSize      :: Int
    , shardCount    :: Int
    , replicaCount  :: Int
    , indexName     :: Text
    , mappingName   :: Text
    , slackExports  :: [FilePath]
    } deriving (Show, Eq)

opts :: ParserInfo SlackOpts
opts = info
    (helper <*> optParser)
    (fullDesc <> progDesc
    "Parses Slack export archives into ElasticSearch for exploring with Kibana")

optParser :: Parser SlackOpts
optParser = SlackOpts
    <$> option text (long "es-url" 
        <> short 'e'
        <> value "http://localhost:9200"
        <> metavar "URL"
        <> help "Target ElasticSearch URL."
        <> showDefault)
    <*> option auto (long "bulk-size"
        <> short 'b'
        <> value 1000
        <> metavar "SIZE"
        <> help "Size of the vector buffered to ElasticSearch via the bulk API."
        <> showDefault)
    <*> option auto (long "shards"
        <> short 's'
        <> value 5
        <> metavar "COUNT"
        <> help "Number of primary shards."
        <> showDefault)
    <*> option auto (long "replicas"
        <> short 'r'
        <> value 1
        <> metavar "COUNT"
        <> help "Number of replica shards."
        <> showDefault)
    <*> option text (long "index"
        <> short 'i'
        <> value "slack"
        <> metavar "NAME"
        <> help "Name of Slack index."
        <> showDefault)
    <*> option text (long "mapping"
        <> short 'm'
        <> value "events"
        <> metavar "NAME"
        <> help "Name of Slack index mapping."
        <> showDefault)
    <*> many (strArgument (metavar "EXPORT_ZIP_FILE(s)..."))
