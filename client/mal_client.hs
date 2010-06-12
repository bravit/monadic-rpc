import Network
import System.IO
import qualified Data.ByteString as BS
import DDefs
import Data.Serialize

main = do
    h <- connectTo "localhost" (PortNumber 1500)
    let msg = BS.pack [1,6,3,4]
    BS.hPut h msg
    hClose h
     
