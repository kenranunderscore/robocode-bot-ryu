{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE OverloadedRecordDot #-}
{-# LANGUAGE OverloadedStrings #-}

module Main where

import Control.Monad
import Data.Aeson ((.=))
import Data.Aeson qualified as Json
import Data.Aeson.KeyMap qualified as KM
import Data.ByteString.Lazy qualified as LBS
import Network.WebSockets
import System.Environment qualified as Env
import System.IO (BufferMode (..), hSetBuffering, stdout)

app :: Connection -> IO b
app conn = do
    secret <- Env.getEnv "SERVER_SECRET"
    putStrLn "connected!"

    -- receive server handshake
    evt <- receiveEvent conn
    putStrLn "\n"
    print evt
    let Just sessionId = KM.lookup "sessionId" evt.payload
    putStrLn "\n"

    -- send bot handshake
    let botHandshake =
            Json.object
                [ "type" .= Json.String "BotHandshake"
                , "sessionId" .= sessionId
                , "name" .= Json.String "Ryu"
                , "version" .= Json.String "0.1"
                , "authors" .= Json.toJSON ["Johannes Maier" :: String]
                , "secret" .= secret
                ]
    putStrLn $ "Sending handshake: " <> show (Json.encode botHandshake)
    sendTextData conn $ Json.encode botHandshake

    printNextMessage conn

    let botReady =
            Json.object ["type" .= Json.String "BotReady"]
    sendTextData conn $ Json.encode botReady

    mainLoop conn

mainLoop :: Connection -> IO b
mainLoop conn = forever $ do
    evt <- receiveEvent conn
    print evt

newtype Event = Event {payload :: KM.KeyMap Json.Value}
    deriving newtype (Show, Json.FromJSON)

receiveEvent :: Connection -> IO Event
receiveEvent conn = do
    raw <- receiveData conn
    case Json.eitherDecode' raw of
        Left err -> fail $ show err
        Right (Json.Object o) ->
            pure $ Event o
        Right unexpected ->
            fail $ "Unexpected event JSON: " <> show unexpected

printNextMessage :: Connection -> IO ()
printNextMessage conn = do
    msg <- receiveData conn
    putStrLn "\n"
    LBS.putStr msg
    putStrLn "\n"

main :: IO ()
main = do
    hSetBuffering stdout LineBuffering
    runClient "localhost" 7654 "/" app
