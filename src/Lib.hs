module Lib
    ( run
    ) where
    
import qualified System.Process.Typed  as P


run :: IO ()
run = do
    P.runProcess_ "rtl_biast -b 1"
    P.withProcessWait_ "dump1090 --device-type rtlsdr --net --forward-mlat" $ \_dump1090 ->
        P.withProcessWait_ "socat -u TCP:localhost:30005 TCP:feed.adsbexchange.com:30005" $ \_socat ->
            return ()