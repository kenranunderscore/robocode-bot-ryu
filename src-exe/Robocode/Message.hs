{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}

module Robocode.Message (
    Message (..),
) where

import Data.Aeson ((.:))
import Data.Aeson qualified as Json
import Data.Text (Text)
import Data.Text qualified as Text

newtype SessionId = SessionId Text
    deriving newtype (Eq, Show, Json.FromJSON, Json.ToJSON)

data Message
    = BotReady
    | RoundStartedEvent
    | ServerHandshake SessionId
    | Unknown Text
    deriving stock (Eq, Show)

instance Json.FromJSON Message where
    parseJSON = Json.withObject "Message" $ \o -> do
        eventType <- o .: "type"
        Json.withText
            "type"
            ( \case
                "BotReady" -> pure BotReady
                "RoundStartedEvent" -> pure RoundStartedEvent
                "ServerHandshake" -> ServerHandshake <$> o .: "sessionId"
                t -> pure $ Unknown t
            )
            eventType
