module CommonUtils where

import DDefs

import qualified Data.ByteString as BS
import Data.Serialize
import Data.Serialize.Put
import Data.Serialize.Get
import System.IO
import System.IO.Error

buildMsgEnv payload = 
        runPut $ do 
            putWord64be size
            putByteString payload
        where size = (fromIntegral $ BS.length payload) :: Word64

failOnEOF connhdl = do
    isEOF <- hIsEOF connhdl
    if not isEOF
        then return ()
        else fail "Connection is closed unexpectedly"

recvMsgEnv h = do
        sz_msg <- BS.hGet h DDefs.msgSizeField
        either (fail . ("Format error: " ++ )) (BS.hGet h . fromIntegral) (runGet getWord64be sz_msg)

main = try (recvMsgEnv stdout)


