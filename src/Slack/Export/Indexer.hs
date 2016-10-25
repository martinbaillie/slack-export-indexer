{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE RecordWildCards #-}

module Slack.Export.Indexer
    (runIndexer)
        where

import              Codec.Archive.Zip               (Archive
                                                    , ZipOption(OptDestination)
                                                    , extractFilesFromArchive
                                                    , toArchive)

import              Control.Monad                   (void, (<=<))
import              Control.Monad.Catch             (Handler(..))
import              Control.Monad.IO.Class          (liftIO)
import              Control.Monad.Trans.Class       (lift, MonadTrans)
import              Control.Monad.Trans.Resource    (runResourceT, ResourceT)

import              Control.Retry                   (recovering
                                                    , exponentialBackoff
                                                    , limitRetries)

import              Data.Aeson                      (Value)
import              Data.Aeson.Encode.Pretty        (encodePretty', Config(..)
                                                    , Indent(Spaces)
                                                    , NumberFormat(Generic))

import qualified    Data.ByteString.Lazy            as LB

import              Database.Bloodhound
import              Data.Conduit                    (awaitForever, ConduitM(..))
import              Data.Conduit.Async              (($$&), (=$&))
import qualified    Data.Conduit.Combinators        as C

import              Data.Digest.Pure.SHA            (showDigest, sha1)
import              Data.Monoid                     ((<>))
import              Data.Text                       (unpack, pack)
import              Data.Vector                     (Vector)

import              Network.HTTP.Client             (defaultManagerSettings
                                                    , HttpException)

import              System.Environment              (getArgs)
import              System.Directory                (doesFileExist
                                                    , withCurrentDirectory)
import              System.IO.Error                 (catchIOError)
import              System.IO.Temp                  (withSystemTempDirectory)

import              Slack.Export.Types
import              Slack.Export.Opts


runIndexer :: SlackOpts -> IO ()
runIndexer opts = mapM_ (index opts) (slackExports opts)

-- | The guts of this little CLI tool
index
    :: SlackOpts
    -> FilePath
    -> IO ()
index SlackOpts{..} file = withSystemTempDirectory "slack-export" (\dir -> do
        zipArchive <- toArchive <$> LB.readFile file
        extractFilesFromArchive [OptDestination dir] zipArchive
        withCurrentDirectory dir $ do
            -- TODO: Thread users hashmap through conduit using ReaderT monad?
            users <- parseSlackUserFile "users.json"

            -- FIXME: Don't create/put if exists
            _ <- es $ createIndex esIndexSettings esIndex
            _ <- es $ putMapping esIndex esMapping SlackMapping

            -- TODO: Thread logging and duration information through the conduit
            runResourceT
                $ C.sourceDirectoryDeep False "."
                  =$& C.filter isSlackMsgFile
                  =$& slackEventParsingConduit
                  =$& C.map (enrichSlackEventWithUserNames users)
                  =$& C.map esMakeDoc
                  =$& C.map (uncurry $ BulkIndex esIndex esMapping)
                  =$& C.conduitVector bulkSize
                  $$& C.mapM_ esBulkIndex)
                  {-$$& C.mapM_ (liftIO . putStrLn . unpack . _message ))-}
        where
            es                = withBH defaultManagerSettings esServer
            esMapping         = MappingName mappingName
            esServer          = Server esUrl
            esIndex           = IndexName indexName
            esIndexSettings   = IndexSettings   (ShardCount shardCount)
                                                (ReplicaCount replicaCount)

            -- | Indexes a vector of ops to ElasticSearch via the bulk index API
            -- | Exponentially backs off upon exception
            esBulkIndex :: MonadTrans mt => Vector BulkOperation -> mt IO ()
            esBulkIndex ops = lift . void . backingOffRetry . es $ bulk ops

-- | Encodes an ElasticSearch doc with deterministic ids from the SlackEvent
esMakeDoc
    :: SlackEvent
    -> (DocId, Value)
esMakeDoc event = (genDocId event, toJSON event) where
    -- Detemnistic ids based on packet content hash
    genDocId = DocId . pack . showDigest . sha1 . encodePretty' deterministic
    deterministic = Config { confIndent = Spaces 0
                           , confCompare = compare
                           , confNumFormat = Generic }

-- | A backing off retry combinator for the ElasticSearch calls
backingOffRetry
    :: IO a
    -> IO a
backingOffRetry req = recovering policy [handler] retryStatus where
    -- Exponential backoff: 10 attempts starting at a 1 second delay
    policy          = exponentialBackoff (1 ^ (6 :: Integer)) <> limitRetries 10
    handler _       = Handler $ \(_ :: HttpException) -> return True
    retryStatus rs  = do
        -- TODO: Request count and better output on how much is being streamed
        putStrLn "Making request..."
        req

-- | Conduit pipeline for producing SlackEvents downstream
slackEventParsingConduit
  :: ConduitM FilePath SlackEvent (ResourceT IO) ()
slackEventParsingConduit = awaitForever $
    maybe (return ()) (\cms ->
        C.yieldMany $ map (mkSlackEvent $ fst cms) (snd cms))
    <=< liftIO . parseSlackChanMsgFile
