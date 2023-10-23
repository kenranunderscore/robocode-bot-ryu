{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE OverloadedRecordDot #-}
{-# LANGUAGE OverloadedStrings #-}

module Main where

import Control.Monad
import Data.Aeson ((.=))
import Data.Aeson qualified as Json
import Data.IORef
import Network.WebSockets qualified as WS
import System.Environment qualified as Env
import System.IO (BufferMode (..), hSetBuffering, stdout)

import Robocode.Message

app :: WS.Connection -> IO b
app conn = do
    secret <- Env.getEnv "SERVER_SECRET"
    putStrLn "connected!"

    -- receive server handshake
    ServerHandshake sessionId <- receiveMessage conn
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
    WS.sendTextData conn $ Json.encode botHandshake

    printNextMessage conn

    let botReady =
            Json.object ["type" .= Json.String "BotReady"]
    WS.sendTextData conn $ Json.encode botReady

    mainLoop conn

mainLoop :: WS.Connection -> IO b
mainLoop conn = do
    turnSignRef <- newIORef @Double 1.0
    forever $ do
        msg <- receiveMessage conn
        case msg of
            TickEventForBot e -> do
                print e.botState
                unless (null e.events) $
                    modifyIORef' turnSignRef (* (-1))
                sign <- readIORef turnSignRef
                let intent =
                        Json.object
                            [ "targetSpeed" .= Json.Number 1.0
                            , "turnRate" .= (sign * 30.0)
                            , "type" .= Json.String "BotIntent"
                            , "firepower" .= mkFirepower 0.2
                            ]
                WS.sendTextData conn $ Json.encode intent
            _ -> print msg

receiveMessage :: WS.Connection -> IO Message
receiveMessage conn = do
    raw <- WS.receiveData conn
    case Json.eitherDecode' raw of
        Left err -> fail $ show err
        Right msg -> pure msg

printNextMessage :: WS.Connection -> IO ()
printNextMessage conn = do
    msg <- receiveMessage conn
    print msg

main :: IO ()
main = do
    hSetBuffering stdout LineBuffering
    WS.runClient "localhost" 7654 "/" app
