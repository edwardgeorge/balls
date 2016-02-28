module CmdOptions (Options(..),
                   readEndpoint,
                   parseOptions) where
import Options.Applicative  -- from optparse-applicative

readEndpoint :: ReadM (String, String)
readEndpoint = eitherReader go
  where go []         = Left "empty endpoint string"
        go (':':_)    = Left "no endpoint specified before ':'"
        go (_:[])     = Left "endpoint missing ':'"
        go (_:':':[]) = Left "no filename given after ':'"
        go (a:':':b)  = Right ([a], b)
        go (a:b)      = (\(x, y) -> (a:x, y)) <$> go b

data Options = Options { getPort :: Int
                       , getHost :: String
                       , imageFile :: String
                       , extraImages :: [(String, String)]}
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
                  <*> (many $ option readEndpoint (long "endpoint"
                                                <> short 'e'
                                                <> metavar "PATH:FILE"))

parseOptions :: IO Options
parseOptions = execParser $ info (helper <*> options) fullDesc
