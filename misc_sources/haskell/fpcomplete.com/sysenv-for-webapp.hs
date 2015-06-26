import Happstack.Server.Env
import System.Environment

main = do
    environment <- getEnvironment
    simpleHTTP nullConf $ ok $ show environment
