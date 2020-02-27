module Lib
    ( run
    ) where
    
import qualified System.Process.Typed  as P


run :: IO ()
run = do
    P.runProcess_ "rtl_biast -b 1"
    P.withProcessWait_ "dump1090 --device-type rtlsdr --net --forward-mlat" $ \_dump1090 ->
        return ()