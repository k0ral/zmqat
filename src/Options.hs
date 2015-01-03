module Options (Input(..), Pattern(..), parseOptions) where

-- {{{ Imports
import           ClassyPrelude       hiding ((<>))

import           Data.Version

import           Options.Applicative

import qualified Paths_zmqat         as ZMQAT

import           System.IO.Unsafe
import qualified System.ZMQ4         as ZMQ (version)
-- }}}

data Input = Input Pattern Text
data Pattern = Req Text | Rep Text | Dealer Text | Router Text | Pub Text | Sub Text [Text] | Push Text | Pull Text

parseOptions :: IO Input
parseOptions = execParser $ info (helper <*> version <*> (Input <$> pattern <*> identity)) $ fullDesc

versionText :: Text
versionText = unsafePerformIO $ do
  (a, b, c) <- liftIO ZMQ.version
  return $ intercalate "\n"
    [ "zmqat binary: v" ++ pack (showVersion ZMQAT.version)
    , "0MQ library: v" ++ intercalate "." (map tshow [a, b, c])
    ]

version :: Parser (a -> a)
version = infoOption (unpack versionText) (short 'V' <> long "version" <> help "Print version info." <> hidden)

pattern :: Parser Pattern
pattern = subparser $ foldl' (<>) mempty [req, rep, dealer, router, pub, sub, push, pull]

identity :: Parser Text
identity = pack <$> strOption (short 'i' <> long "identity" <> metavar "IDENTITY" <> help "Socket identity.")

req :: Mod CommandFields Pattern
req = command "req" $ info options $ progDesc "Requester"
  where options = Req . pack <$> strArgument
                  (  metavar "TARGET_SOCKET"
                  <> help "Requests will be listened from TARGET_SOCKET." )

rep :: Mod CommandFields Pattern
rep = command "rep" $ info options $ progDesc "Responser"
  where options = Rep . pack <$> strArgument
                  (  metavar "TARGET_SOCKET"
                  <> help "Requests will be sent to TARGET_SOCKET." )

dealer :: Mod CommandFields Pattern
dealer = command "dealer" $ info options $ progDesc "Dealer"
  where options = Dealer . pack <$> socketOption
        socketOption = strArgument
                       (  metavar "TARGET_SOCKET"
                       <> help "Requests will be sent to TARGET_SOCKET." )

router :: Mod CommandFields Pattern
router = command "router" $ info options $ progDesc "Router"
  where options = Router . pack <$> strArgument
                  (  metavar "TARGET_SOCKET"
                  <> help "?" )

pub :: Mod CommandFields Pattern
pub = command "pub" $ info options $ progDesc "Publisher"
  where options = Pub . pack <$> strArgument
                  (  metavar "TARGET_SOCKET"
                  <> help "Messages will be published to TARGET_SOCKET." )

sub :: Mod CommandFields Pattern
sub = command "sub" $ info options $ progDesc "Subscriber"
  where options = Sub <$> socketArg <*> some subscriptionArg
        socketArg = pack <$> strArgument (metavar "TARGET_SOCKET" <> help "Subscribe to messages sent to TARGET_SOCKET.")
        subscriptionArg = pack <$> strArgument (metavar "SUBSCRIPTION")

push :: Mod CommandFields Pattern
push = command "push" $ info options $ progDesc "Pusher"
  where options = Push . pack <$> strArgument
                  (  metavar "TARGET_SOCKET"
                  <> help "Requests will be listened from TARGET_SOCKET." )

pull :: Mod CommandFields Pattern
pull = command "pull" $ info options $ progDesc "Puller"
  where options = Pull . pack <$> strArgument
                  (  metavar "TARGET_SOCKET"
                  <> help "Requests will be sent to TARGET_SOCKET." )
