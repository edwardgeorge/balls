module CmdOptions (Options(..),
                   parseOptions) where
import Options.Applicative

data Options = Options { getPort :: Int
                       , getHost :: String
                       , imageFile :: String }
             deriving (Show)

options :: Parser Options
options = Options <$> option auto (long "port"
                                <> short 'p'
                                <> metavar "PORT")
                  <*> option auto (long "host"
                                <> short 'H'
                                <> metavar "IPADDR"
                                <> value "127.0.0.1")
                  <*> argument str (metavar "FILE")

parseOptions :: IO Options
parseOptions = execParser $ info (helper <*> options) fullDesc
