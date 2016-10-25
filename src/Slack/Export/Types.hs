{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE OverloadedStrings #-}

module Slack.Export.Types
    ( SlackEvent(..)
    , SlackUsers
    , SlackMapping(..)

    , mkSlackEvent

    , parseSlackMsgFile
    , parseSlackUserFile
    , parseSlackChanMsgFile

    , enrichSlackEventWithUserNames
    , isSlackMsgFile
    ) where

import              Control.Monad               ((>>=), mzero)

import              Data.Aeson
import              Data.Aeson.TH               (deriveJSON,
                                                fieldLabelModifier,
                                                defaultOptions)
import              Data.Aeson.Types            (Parser)
import              Data.Char                   (toLower)
import qualified    Data.ByteString.Lazy        as LB
import qualified    Data.HashMap.Strict         as HM
import              Data.List                   (find, isSuffixOf)
import              Data.Text                   (pack, unpack, splitOn, Text)
import qualified    Data.Vector                 as V

import              System.FilePath             (takeDirectory, takeBaseName
                                                , takeFileName)
import              Text.Regex                  (mkRegex, matchRegexAll, Regex)

--------------------------------------------------------------------------------
--  Types

data SlackMapping = SlackMapping deriving (Eq, Show)
instance ToJSON SlackMapping where
  toJSON SlackMapping =
    object ["events"        .=
      object ["properties"  .=
        object ["timestamp" .=
          object ["type"    .= ("date" :: Text),
                  "format"  .= ("epoch_second" :: Text)]]]]

type SlackChannel = Text

data SlackMessage = SlackMessage
    { msgType       :: Text
    , msgSubType    :: Maybe Text
    , msgUser       :: Text
    , msgText       :: Text
    , msgTs         :: Text
    } deriving (Show, Eq)
$(deriveJSON defaultOptions 
    {fieldLabelModifier = map toLower . drop 3} ''SlackMessage)

type SlackUserId    = Text
type SlackUserName  = Text
data SlackUser      = SlackUser
    { usrId         :: SlackUserId
    , usrName       :: SlackUserName
    } deriving (Show, Eq)
instance FromJSON SlackUser where
    parseJSON (Object v) = SlackUser <$> v .: "id" <*> v .: "name"
    parseJSON _ = mzero

type SlackUserMap   = HM.HashMap SlackUserId SlackUser
newtype SlackUsers  = SlackUsers SlackUserMap deriving (Show)
instance FromJSON SlackUsers where
  parseJSON (Array vs) =
      fmap SlackUsers $ V.foldl insertUser (return HM.empty) vs
      where
        insertUser phm v = do
            user <- parseJSON v :: Parser (Maybe SlackUser)
            maybe phm (\u -> HM.insert (usrId u) u <$> phm) user
  parseJSON _ = mzero

data SlackEvent = SlackEvent
    { _channel   :: SlackChannel
    , _username  :: Maybe SlackUserName
    , _userid    :: SlackUserId
    , _message   :: Text
    , _type      :: Text
    , _subType   :: Maybe Text
    , _timestamp :: Text
    } deriving (Show, Eq)
$(deriveJSON defaultOptions
    {fieldLabelModifier = drop 1} ''SlackEvent)

--------------------------------------------------------------------------------
--  Loosely related Slack functions

-- | File paths matching Slack message filename format
isSlackMsgFile
    :: FilePath
    -> Bool
isSlackMsgFile p = let f = takeFileName p in
                        head f /= '.' &&
                        ".json" `isSuffixOf` f &&
                        (not $ f `elem`
                        ["users.json","channels.json","integration_logs.json"])

-- | Decode an individual Slack message file
parseSlackMsgFile
    :: FilePath
    -> IO (Maybe [SlackMessage])
parseSlackMsgFile f = (\bs ->
    decode bs :: Maybe [SlackMessage]) <$> LB.readFile f

-- | Tuple of SlackChannel:[SlackMessage] parsed and extracted from the filepath
parseSlackChanMsgFile
  :: FilePath -> IO (Maybe (SlackChannel, [SlackMessage]))
parseSlackChanMsgFile f = do
    msgs <- parseSlackMsgFile f
    return $ (,) (pack . takeBaseName $ takeDirectory f) <$> msgs

-- | Decode an individual Slack users file
-- | Futile to continue the index process if fails, so bubbles up an error
parseSlackUserFile
    :: FilePath
    -> IO SlackUsers
parseSlackUserFile f = do
    raw <- eitherDecode <$> LB.readFile f :: IO (Either String SlackUsers)
    case raw of
        Left e  -> error e
        Right u -> return u

-- | Build a SlackEvent for an individual message in a channel
-- | Note: drops the unique identifying value Slack adds to the timestamp
-- | TODO: drop this value during JSON parsing stage instead
mkSlackEvent
    :: SlackChannel
    -> SlackMessage
    -> SlackEvent
mkSlackEvent c m = SlackEvent c Nothing
    (msgUser m)
    (msgText m)
    (msgType m)
    (msgSubType m)
    (head $ splitOn "." (msgTs m))

-- | Enrich the SlackEvent with username data from the SlackUsers hashmap
enrichSlackEventWithUserNames
    :: SlackUsers
    -> SlackEvent
    -> SlackEvent
enrichSlackEventWithUserNames (SlackUsers users) event = event {
    -- Add the username to the event based on the userid
    _username   = usrName <$> HM.lookup (_userid event) users,
    -- Substitute ids for usernames within the message text (i.e. @mentions)
    _message    = pack $ subIdsForUsernames (unpack $ _message event) }
        -- Haskell's string situation is a real PITA... pack unpack pack...
        -- FIXME: Find a full unicode regex lib that works on Data.Text
        where slackUsernameRegex = mkRegex "U[0-9][A-Z0-9]{7}"
              subIdsForUsernames s = case matchRegexAll slackUsernameRegex s of
                Nothing -> s
                Just (head, uid, tail, _) -> head
                    ++ maybe uid (unpack . usrName) (HM.lookup (pack uid) users)
                    ++ subIdsForUsernames tail
