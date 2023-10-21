{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE NoFieldSelectors #-}

module Robocode.Message (
    Firepower,
    Message (..),
    TickEventPayload (..),
    mkFirepower,
) where

import Data.Aeson ((.:))
import Data.Aeson qualified as Json
import Data.Text (Text)

newtype SessionId = SessionId Text
    deriving newtype (Eq, Show, Json.FromJSON, Json.ToJSON)

data Message
    = BotReady
    | RoundStartedEvent
    | ServerHandshake SessionId
    | BotIntent BotIntentPayload
    | TickEventForBot TickEventPayload
    | Unknown Text
    deriving stock (Eq, Show)

data TickEventPayload = TickEventPayload
    { roundNumber :: Int
    , enemyCount :: Int
    , -- , botState :: ?
      -- , bulletStates :: ?
      events :: [Json.Value]
    }
    deriving stock (Eq, Show)

newtype Firepower = Firepower Double
    deriving stock (Eq, Show)

mkFirepower :: Double -> Maybe Firepower
-- FIXME: why do the docs say >= 0?
mkFirepower p
    | p > 0 && p <= 3 = Just (Firepower p)
    | otherwise = Nothing

-- FIXME: all fields are basically optional
data BotIntentPayload = BotIntentPayload
    { turnRate :: Double
    , gunTurnRate :: Double
    , radarTurnRate :: Double
    , targetSpeed :: Double
    , firepower :: Firepower
    , adjustGunForBodyTurn :: Bool
    , adjustRadarForBodyTurn :: Bool
    , adjustRadarForGunTurn :: Bool
    , rescan :: Bool
    , fireAssist :: Bool
    , -- , bodyColor :: ?
      -- , turretColor :: ?
      -- , radarColor :: ?
      -- , bulletColor :: ?
      -- , scanColor :: ?
      -- , tracksColor :: ?
      -- , gunColor :: ?
      stdOut :: Text
    , stdErr :: Text
    -- , teamMessages :: ?
    }
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
                "TickEventForBot" -> do
                    payload <-
                        TickEventPayload
                            <$> o .: "roundNumber"
                            <*> o .: "enemyCount"
                            <*> o .: "events"
                    pure $ TickEventForBot payload
                t -> pure $ Unknown t
            )
            eventType
