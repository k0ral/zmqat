module Main where

-- {{{ Imports
import           Options             hiding (Pattern (..))
import qualified Options             (Pattern (..))

import           ClassyPrelude

import           Data.List.NonEmpty  hiding (map)

import           System.IO           (BufferMode (..), hSetBuffering)
import           System.ZMQ4.Monadic as ZMQ hiding (identity, message)
-- }}}

main :: IO ()
main = do
    Input pattern identity <- parseOptions
    hSetBuffering stdout NoBuffering

    case pattern of
      Options.Req a -> requester a identity
      Options.Rep a -> responser a identity
      Options.Dealer a -> dealer a identity
      Options.Router a -> router a identity
      Options.Pub a -> publisher a identity
      Options.Sub a b -> subscriber a identity b
      Options.Push a -> pusher a identity
      Options.Pull a -> puller a identity


requester :: Text -> Text -> IO ()
requester socketName identity = runZMQ $ do
  reqSocket <- socket Req
  setIdentity (restrict $ encodeUtf8 identity) reqSocket
  connect reqSocket $ unpack socketName
  forever $ do
    putStr "> "
    send reqSocket [] . encodeUtf8 =<< getLine
    putStrLn . ("< " ++) . decodeUtf8 =<< receive reqSocket

responser :: Text -> Text -> IO ()
responser socketName identity = runZMQ $ do
  repSocket <- socket Rep
  setIdentity (restrict $ encodeUtf8 identity) repSocket
  bind repSocket $ unpack socketName
  forever $ do
    putStrLn . ("< " ++) . decodeUtf8 =<< receive repSocket
    putStr "> "
    send repSocket [] . encodeUtf8 =<< getLine

router :: Text -> Text -> IO ()
router socketName identity = runZMQ $ do
  routerSocket <- socket Router
  setIdentity (restrict $ encodeUtf8 identity) routerSocket
  bind routerSocket $ unpack socketName
  forever $ do
    input <- receiveMulti routerSocket
    case input of
      identity : "" : request : [] -> do
        putStrLn . ("< " ++) $ decodeUtf8 request
        putStr "> "
        response <- encodeUtf8 <$> getLine
        sendMulti routerSocket $ identity :| "" : response : []
      _ -> putStrLn "Received invalid frames."

dealer :: Text -> Text -> IO ()
dealer socketName identity = runZMQ $ do
  dealerSocket <- socket Dealer
  setIdentity (restrict $ encodeUtf8 identity) dealerSocket
  bind dealerSocket $ unpack socketName
  forever $ putStrLn . decodeUtf8 =<< receive dealerSocket

publisher :: Text -> Text -> IO ()
publisher socketName identity = runZMQ $ do
  pubSocket <- socket Pub
  setIdentity (restrict $ encodeUtf8 identity) pubSocket
  connect pubSocket $ unpack socketName
  forever $ putStr "> " >> getLine >>= send pubSocket [] . encodeUtf8

subscriber :: Text -> Text -> [Text] -> IO ()
subscriber socketName identity subscriptions = runZMQ $ do
  subSocket <- socket Sub
  setIdentity (restrict $ encodeUtf8 identity) subSocket
  bind subSocket $ unpack socketName
  forM subscriptions $ subscribe subSocket . encodeUtf8
  forever $ receive subSocket >>= putStrLn . ("< " ++) . decodeUtf8

pusher :: Text -> Text -> IO ()
pusher socketName identity = runZMQ $ do
  pushSocket <- socket Push
  setIdentity (restrict $ encodeUtf8 identity) pushSocket
  connect pushSocket $ unpack socketName
  forever $ putStr "> " >> getLine >>= send pushSocket [] . encodeUtf8

puller :: Text -> Text -> IO ()
puller socketName identity = runZMQ $ do
  pullSocket <- socket Pull
  setIdentity (restrict $ encodeUtf8 identity) pullSocket
  bind pullSocket $ unpack socketName
  forever $ putStrLn . ("< " ++) . decodeUtf8 =<< receive pullSocket

io :: (MonadIO m) => IO a -> m a
io = liftIO
