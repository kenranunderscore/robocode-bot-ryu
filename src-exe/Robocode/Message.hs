{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE NoFieldSelectors #-}

module Robocode.Message (
    Firepower,
    Message (..),
    TickEventPayload (..),
    mkFirepower,
) where

import Data.Aeson ((.:), (.:?))
import Data.Aeson qualified as Json
import Data.Text (Text)
import Data.Text qualified as T

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

type BotId = Int

newtype TurnNumber = TurnNumber Int
    deriving stock (Eq, Show)
    deriving newtype (Json.FromJSON)

data ScannedBotEventPayload = ScannedBotEventPayload
    { turnNumber :: TurnNumber
    , scannedByBotId :: BotId
    , scannedBotId :: BotId
    , energy :: Double
    , x :: Double
    , y :: Double
    , direction :: Double
    , speed :: Double
    }
    deriving stock (Eq, Show)

newtype Color = Color String
    deriving stock (Eq, Show)
    deriving newtype (Json.FromJSON)

data BulletState = BulletState
    { bulletId :: Int
    , ownerId :: BotId
    , power :: Firepower
    , x :: Double
    , y :: Double
    , direction :: Double
    , color :: Maybe Color
    }
    deriving stock (Eq, Show)

data BulletHitBotEventPayload = BulletHitBotEventPayload
    { turnNumber :: TurnNumber
    , victimId :: BotId
    , damage :: Double
    , energy :: Double
    , bullet :: BulletState
    }
    deriving stock (Eq, Show)

data BulletFiredEventPayload = BulletFiredEventPayload
    { turnNumber :: TurnNumber
    , bullet :: BulletState
    }
    deriving stock (Eq, Show)

data BulletHitWallEventPayload = BulletHitWallEventPayload
    { turnNumber :: TurnNumber
    , bullet :: BulletState
    }
    deriving stock (Eq, Show)

data BotDeathEventPayload = BotDeathEventPayload
    { turnNumber :: TurnNumber
    , victimId :: BotId
    }
    deriving stock (Eq, Show)

data BotHitBotEventPayload = BotHitBotEventPayload
    { turnNumber :: TurnNumber
    , victimId :: BotId
    , botId :: BotId
    , energy :: Double
    , x :: Double
    , y :: Double
    , rammed :: Bool
    }
    deriving stock (Eq, Show)

data BotHitWallEventPayload = BotHitWallEventPayload
    { turnNumber :: TurnNumber
    , victimId :: BotId
    }
    deriving stock (Eq, Show)

data BulletHitBulletEventPayload = BulletHitBulletEventPayload
    { turnNumber :: TurnNumber
    , bullet :: BulletState
    , hitBullet :: BulletState
    }
    deriving stock (Eq, Show)

newtype SkippedTurnEventPayload = SkippedTurnEventPayload
    { turnNumber :: TurnNumber
    }
    deriving stock (Eq, Show)

newtype WonRoundEventPayload = WonRoundEventPayload
    { turnNumber :: TurnNumber
    }
    deriving stock (Eq, Show)

data BotEvent
    = BotDeathEvent BotDeathEventPayload
    | BotHitBotEvent BotHitBotEventPayload
    | BotHitWallEvent BotHitWallEventPayload
    | BulletFiredEvent BulletFiredEventPayload
    | BulletHitBulletEvent BulletHitBulletEventPayload
    | BulletHitWallEvent BulletHitWallEventPayload
    | BulletHitBotEvent BulletHitBotEventPayload
    | ScannedBotEvent ScannedBotEventPayload
    | SkippedTurnEvent SkippedTurnEventPayload
    | WonRoundEvent WonRoundEventPayload
    | TeamMessageEvent
    deriving stock (Eq, Show)

data TickEventPayload = TickEventPayload
    { roundNumber :: Int
    , enemyCount :: Int
    , -- , botState :: ?
      -- , bulletStates :: ?
      events :: [BotEvent]
    }
    deriving stock (Eq, Show)

newtype Firepower = Firepower Double
    deriving stock (Eq, Show)
    deriving newtype (Json.ToJSON)

instance Json.FromJSON Firepower where
    parseJSON v = do
        p <- Json.parseJSON v
        case mkFirepower p of
            Nothing -> fail $ "invalid firepower: " <> show p
            Just power -> pure power

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

instance Json.FromJSON BotEvent where
    parseJSON v =
        Json.withObject
            "BotEvent"
            ( \o -> do
                t <- o .: "type"
                Json.withText
                    "type"
                    ( \case
                        "BotDeathEvent" -> BotDeathEvent <$> Json.parseJSON v
                        "BotHitBotEvent" -> BotHitBotEvent <$> Json.parseJSON v
                        "BotHitWallEvent" -> BotHitWallEvent <$> Json.parseJSON v
                        "BulletFiredEvent" -> BulletFiredEvent <$> Json.parseJSON v
                        "BulletHitBulletEvent" -> BulletHitBulletEvent <$> Json.parseJSON v
                        "BulletHitWallEvent" -> BulletHitWallEvent <$> Json.parseJSON v
                        "BulletHitBotEvent" -> BulletHitBotEvent <$> Json.parseJSON v
                        "ScannedBotEvent" -> ScannedBotEvent <$> Json.parseJSON v
                        "SkippedTurnEvent" -> SkippedTurnEvent <$> Json.parseJSON v
                        "WonRoundEvent" -> WonRoundEvent <$> Json.parseJSON v
                        "TeamMessageEvent" -> pure TeamMessageEvent
                        unknown -> fail $ "unknown bot event: " <> T.unpack unknown
                    )
                    t
            )
            v

instance Json.FromJSON ScannedBotEventPayload where
    parseJSON = Json.withObject "ScannedBotEventPayload" $ \o ->
        ScannedBotEventPayload
            <$> o .: "turnNumber"
            <*> o .: "scannedByBotId"
            <*> o .: "scannedBotId"
            <*> o .: "energy"
            <*> o .: "x"
            <*> o .: "y"
            <*> o .: "direction"
            <*> o .: "speed"

instance Json.FromJSON BulletHitBotEventPayload where
    parseJSON = Json.withObject "BulletHitBotEventPayload" $ \o ->
        BulletHitBotEventPayload
            <$> o .: "turnNumber"
            <*> o .: "victimId"
            <*> o .: "damage"
            <*> o .: "energy"
            <*> o .: "bullet"

instance Json.FromJSON BulletFiredEventPayload where
    parseJSON = Json.withObject "BulletFiredEventPayload" $ \o ->
        BulletFiredEventPayload
            <$> o .: "turnNumber"
            <*> o .: "bullet"

instance Json.FromJSON BulletHitWallEventPayload where
    parseJSON = Json.withObject "BulletHitWallEventPayload" $ \o ->
        BulletHitWallEventPayload
            <$> o .: "turnNumber"
            <*> o .: "bullet"

instance Json.FromJSON BotDeathEventPayload where
    parseJSON = Json.withObject "BotDeathEventPayload" $ \o ->
        BotDeathEventPayload
            <$> o .: "turnNumber"
            <*> o .: "victimId"

instance Json.FromJSON BulletState where
    parseJSON = Json.withObject "BulletState" $ \o ->
        BulletState
            <$> o .: "bulletId"
            <*> o .: "ownerId"
            <*> o .: "power"
            <*> o .: "x"
            <*> o .: "y"
            <*> o .: "direction"
            <*> o .:? "color"

instance Json.FromJSON BotHitBotEventPayload where
    parseJSON = Json.withObject "BotHitBotEventPayload" $ \o ->
        BotHitBotEventPayload
            <$> o .: "turnNumber"
            <*> o .: "victimId"
            <*> o .: "botId"
            <*> o .: "energy"
            <*> o .: "x"
            <*> o .: "y"
            <*> o .: "rammed"

instance Json.FromJSON BotHitWallEventPayload where
    parseJSON = Json.withObject "BotHitWallEventPayload" $ \o ->
        BotHitWallEventPayload
            <$> o .: "turnNumber"
            <*> o .: "victimId"

instance Json.FromJSON SkippedTurnEventPayload where
    parseJSON = Json.withObject "SkippedTurnEventPayload" $ \o ->
        SkippedTurnEventPayload <$> o .: "turnNumber"

instance Json.FromJSON WonRoundEventPayload where
    parseJSON = Json.withObject "WonRoundEventPayload" $ \o ->
        WonRoundEventPayload <$> o .: "turnNumber"

instance Json.FromJSON BulletHitBulletEventPayload where
    parseJSON = Json.withObject "BulletHitBulletEventPayload" $ \o ->
        BulletHitBulletEventPayload
            <$> o .: "turnNumber"
            <*> o .: "bullet"
            <*> o .: "hitBullet"
