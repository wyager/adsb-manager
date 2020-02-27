module Lib
    ( run
    ) where
    
import qualified System.Process.Typed as P
import qualified Control.Concurrent as C

run :: IO ()
run = do
    P.runProcess_ "rtl_biast -b 1"
    P.withProcessWait_ "dump1090 --device-type rtlsdr --net --forward-mlat" $ \_dump1090 -> do
        C.threadDelay (5*1000*1000)
        P.withProcessWait_ "socat -u TCP:localhost:30005 TCP:feed.adsbexchange.com:30005" $ \_socat -> do
            return ()