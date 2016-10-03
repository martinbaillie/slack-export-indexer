{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE OverloadedStrings #-}

module Slack
    ( SlackEvent(..)
    , SlackUser(..)
    , SlackMessage(..)
    , SlackMapping(..)

    , slackEvent

    , parseSlackMsgFile
    , parseSlackUserFile
    , parseSlackChanMsgFile

    , slackUserFromId
    , slackUserNameFromId

    , convertSlackUserIdsWith
    , nonSlackMsgFile
    ) where

import              Data.Aeson
import              Data.Aeson.TH               (deriveJSON,
                                                fieldLabelModifier,
                                                defaultOptions)
import              Data.Char                   (toLower)
import qualified    Data.ByteString.Lazy        as LB
import              Data.List                   (find, isSuffixOf)
import qualified    Data.Text                   as T

import              System.FilePath             (takeDirectory, takeBaseName
                                                , takeFileName)
--------------------------------------------------------------------------------
--  Types

data SlackMapping = SlackMapping deriving (Eq, Show)
instance ToJSON SlackMapping where
  toJSON SlackMapping =
    object ["events"        .=
      object ["properties"  .=
        object ["timestamp" .=
          object ["type"    .= ("date" :: T.Text),
                  "format"  .= ("epoch_second" :: T.Text)]]]]

type SlackChannel = T.Text

data SlackEvent = SlackEvent
    { _channel   :: T.Text
    , _username  :: T.Text
    , _message   :: T.Text
    , _timestamp :: T.Text
    , _type      :: T.Text
    , _subType   :: Maybe T.Text
    } deriving (Show, Eq)
$(deriveJSON defaultOptions
    {fieldLabelModifier = drop 1} ''SlackEvent)

data SlackUser = SlackUser
    {  usrId     :: T.Text
     , usrName   :: T.Text
    } deriving (Show, Eq)
$(deriveJSON defaultOptions
    {fieldLabelModifier = map toLower . drop 3} ''SlackUser)

data SlackMessage = SlackMessage
    { msgType :: T.Text
    , msgSubType :: Maybe T.Text
    , msgUser :: T.Text
    , msgText :: T.Text
    , msgTs   :: T.Text
    } deriving (Show, Eq)
$(deriveJSON defaultOptions
    {fieldLabelModifier = map toLower . drop 3} ''SlackMessage)

--------------------------------------------------------------------------------
--  Loosely related Slack functions

parseSlackUserFile
    :: FilePath
    -> IO [SlackUser]
parseSlackUserFile f = do
    raw <- eitherDecode <$> LB.readFile f :: IO (Either String [SlackUser])
    case raw of
        Left e -> error e
        Right u -> return u

parseSlackMsgFile
    :: FilePath
    -> IO (Maybe [SlackMessage])
parseSlackMsgFile f = (\bs ->
    decode bs :: Maybe [SlackMessage]) <$> LB.readFile f

parseSlackChanMsgFile
  :: FilePath -> IO (Maybe (SlackChannel, [SlackMessage]))
parseSlackChanMsgFile f = do
    msgs <- parseSlackMsgFile f
    return $ (,) (T.pack . takeBaseName $ takeDirectory f) <$> msgs

slackUserFromId
    :: T.Text
    -> [SlackUser]
    -> Maybe SlackUser
slackUserFromId id = find (\u -> usrId u == id)

slackUserNameFromId
    :: T.Text
    -> [SlackUser]
    -> T.Text
slackUserNameFromId uid users = maybe uid usrName (slackUserFromId uid users)

convertSlackUserIdsWith
    :: [SlackUser]
    -> SlackEvent
    -> SlackEvent
convertSlackUserIdsWith users e = e {
    _username = slackUserNameFromId (_username e) users }

nonSlackMsgFile
    :: FilePath
    -> Bool
nonSlackMsgFile p = let f = takeFileName p in
                        head f == '.' ||
                        not (".json" `isSuffixOf` f) ||
                        elem f [ "users.json"
                               , "channels.json"
                               , "integration_logs.json" ]
slackEvent
    :: SlackChannel
    -> SlackMessage
    -> SlackEvent
-- TODO: find/replace usernames inside message text
slackEvent c m = SlackEvent c
    (msgUser m)
    (msgText m)
    (head $ T.splitOn "." (msgTs m))
    (msgType m)
    (msgSubType m)
