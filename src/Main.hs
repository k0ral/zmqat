module Main where

import Control.Applicative
import Control.Monad (forever, when)
import Data.Function (fix)
import System.Environment
import System.ZMQ
import Data.ByteString.Char8 (pack, unpack)
import Data.ByteString.UTF8 (fromString)


main :: IO ()
main = withContext 1 $ \context -> do  
    args <- getArgs
    case args of
        ["-h"]      -> quickHelp
        ["--help"]  -> quickHelp


        ["-req", socketName, message] ->  
            withSocket context Req $ \socket -> do
                connect socket socketName
                send socket (pack message) []                
                reply <- receive socket []
                putStrLn $ unpack reply

        ["-reqi", socketName] ->
            withSocket context Req $ \socket -> do
                connect socket socketName
                forever $ do
                    line <- fromString <$> getLine
                    send socket line []
                    reply <- receive socket []
                    putStrLn $ unpack reply

        ["-rep", socketName, defaultResponse] -> 
            withSocket context Rep $ \socket -> do
                bind socket socketName 
                forever $ do
                    message <- receive socket []
                    putStrLn $ unpack message
                    send socket (pack defaultResponse) []

        ["-rbrok", frontendSocket, backendSocket] -> 
            withSocket context Xrep $ \frontend -> 
                withSocket context Xreq $ \backend -> do
                    bind frontend frontendSocket
                    bind backend  backendSocket 
                    device Queue frontend backend

        ["-sub", socketName, filter] ->
            withSocket context Sub $ \socket -> do
                connect socket socketName
                subscribe socket filter
                forever $ do
                    message <- receive socket []
                    putStrLn $ unpack message

        ["-pub", socketName] -> 
            withSocket context Pub $ \socket -> do
                bind socket socketName
                forever $ do
                    line <- fromString <$> getLine
                    send socket line []

        ["-proxy", frontendSocket, backendSocket, filter] -> 
            withSocket context Sub $ \frontend -> 
                withSocket context Pub $ \backend -> do
                    connect frontend frontendSocket
                    subscribe frontend filter 
                    bind backend backendSocket
                    forever $ proxy frontend backend
                
        _ -> do
            putStrLn "Wrong arguments given."
            quickHelp
            
            
            return ()

quickHelp :: IO ()
quickHelp = do
    putStrLn "Usage: zmqat [-req|-reqi-rep|-rbrok|-sub--pub|-proxy] [OPTIONS]"
    putStrLn ""
    putStrLn "\"-req SOCKET_URI MESSAGE\" : send a single MESSAGE to the given responder SOCKET_URI."
    putStrLn "\"-reqi SOCKET_URI\" : connect to responder SOCKET_URI and prompt interactively the user for messages to send."
    putStrLn "\"-rep SOCKET_URI DEFAULT_RESPONSE\" : listen to requests at SOCKET_URI and send DEFAULT_RESPONSE to requesters."
    putStrLn "\"-rbrok FRONTEND_SOCKET_URI BACKEND_SOCKET_URI\" : forward all requests received from FRONTEND_SOCKET_URI to BACKEND_SOCKET_URI. Doesn't seem to work for now."
    putStrLn "\"-sub SOCKET_URI FILTER\" : connect to a publisher SOCKET_URI and print every published message that matches the FILTER."
    putStrLn "\"-pub SOCKET_URI\" : prompt interactively the user for messages to publish at socket SOCKET_URI"
    putStrLn "\"-proxy FRONTEND_SOCKET_URI BACKEND_SOCKET_URI FILTER\" : forward all published messages from BACKEND_SOCKET_URI to FRONTEND_SOCKET_URI while filtering them with FILTER."


proxy from to = fix $ \loop -> do
    message <- receive from []
    more <- moreToReceive from 
    send to message [SndMore | more]
    when more loop
