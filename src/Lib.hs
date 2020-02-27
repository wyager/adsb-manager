module Lib
    ( run
    ) where
    
import qualified System.Process.Typed as P
import qualified Control.Concurrent as C
import           Data.Text (Text)
import qualified Data.Text as Text
import qualified Data.Text.IO as TIO
import           Data.Time.Calendar (Day)
import qualified Data.Time.Calendar as Cal
import           Data.Time.Clock (DiffTime)
import qualified Data.Time.Clock as Clock
import           GHC.Generics (Generic)
import qualified Data.Csv as Csv
import           Control.Monad (mzero)
import qualified Data.Attoparsec.Text as AT
import qualified Data.ByteString.Lazy as BS
import           Data.Vector (Vector)
import           Control.Applicative ((<|>))
run :: IO ()
run = do
    input <- TIO.getContents
    mapM_ (either print print . AT.parseOnly (parser @BaseStationRow)) $ Text.lines input
    

runCommands :: IO ()
runCommands = do
    P.runProcess_ "rtl_biast -b 1"
    P.withProcessWait_ "dump1090 --device-type rtlsdr --net --forward-mlat" $ \_dump1090 -> do
        C.threadDelay (5*1000*1000)
        P.withProcessWait_ "socat -u TCP:localhost:30005 TCP:feed.adsbexchange.com:30005" $ \_socat -> do
            return ()


data MsgType = SEL | ID | AIR | STA | CLK | MSG deriving (Show, Generic)
instance HasParser MsgType where
    parser = 
        (SEL <$ "SEL") <|>
        (ID <$ "ID") <|>
        (AIR <$ "AIR") <|>
        (STA <$ "STA") <|>
        (CLK <$ "CLK") <|>
        (MSG <$ "MSG") 


data TransmissionType = T1 | T2 | T3 | T4 | T5 | T6 | T7 | T8 deriving (Show)
instance HasParser TransmissionType where
    parser = 
        (T1 <$ "1") <|>
        (T2 <$ "2") <|>
        (T3 <$ "3") <|>
        (T4 <$ "4") <|>
        (T5 <$ "5") <|>
        (T6 <$ "6") <|>
        (T7 <$ "7") <|>
        (T8 <$ "8")

newtype BSDay = BSDay Day deriving (Show)
newtype BSDiffTime = BSDiffTime DiffTime deriving (Show)


class HasParser a where
    parser :: AT.Parser a

instance HasParser BSDay where
    parser = do
        year <- AT.decimal
        month <- "/" *> AT.decimal
        day <- "/" *> AT.decimal
        return $ BSDay (Cal.fromGregorian year month day)

instance HasParser BSDiffTime where
    parser = do
        hours <- AT.decimal
        minutes <- ":" *> AT.decimal
        seconds <- ":" *> AT.rational
        return $ BSDiffTime (Clock.secondsToDiffTime (hours * 60 * 60 + minutes * 60) + seconds)

-- usingParser :: HasParser a => Csv.Field -> Csv.Parser a
-- usingParser bs = case AT.parseOnly parser bs of
--     Left _err -> mzero
--     Right a -> pure a

-- instance Csv.FromField BSDay where
--     parseField = usingParser
-- instance Csv.FromField BSDiffTime where
--     parseField = usingParser


data BaseStationRow = Row
           { msgType :: !(MsgType)
           , transmissionType :: !TransmissionType
           , sessionID :: !Text
           , aircraftID :: !Text
           , hexIdent :: !Text
           , flightID :: !Text
           , dateGenerated :: !BSDay
           , timeGenerated :: !BSDiffTime
           , dateLogged :: !BSDay
           , timeLogged :: !BSDiffTime
           , callsign :: !Text
           , altitude :: !Text
           , groundSpeed :: !Text
           , track :: !Text
           , latitude :: !Text
           , longitude :: !Text
           , verticalRate :: !Text
           , squawk :: !Text
           , alert :: !Text
           , emergency :: !Text
           , sPI :: !Text
           , isOnGround :: !Text
           } deriving (Show, Generic)

text :: AT.Parser Text
text = AT.takeWhile (/= ',')
instance HasParser BaseStationRow where
    parser = Row <$> parser 
        <*> c parser 
        <*> c text 
        <*> c text 
        <*> c text 
        <*> c text 
        <*> c parser 
        <*> c parser 
        <*> c parser 
        <*> c parser 
        <*> c text 
        <*> c text 
        <*> c text 
        <*> c text 
        <*> c text 
        <*> c text 
        <*> c text 
        <*> c text 
        <*> c text 
        <*> c text 
        <*> c text 
        <*> c text 
        where 
        c :: AT.Parser a -> AT.Parser a
        c p = "," *> p
