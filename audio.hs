-- Sin waves for music 
-- credit: https://www.youtube.com/watch?v=FYTZkE5BZ-0&t=91s

import qualified Data.ByteString.Lazy as B
import qualified Data.ByteString.Builder as B
import Data.Foldable 
import System.Process
-- formatting string 
import Text.Printf

outputFilePath:: FilePath
outputFilePath = "output.bin"

outputFileScale:: FilePath
outputFileScale = "scale.bin"

type Pulse = Float
type Seconds = Float
type Samples = Float
type Hz = Float
type Semitones = Float

sampleRate :: Samples
sampleRate = 48000.00

pitchStandard :: Hz
pitchStandard = 440.0

f :: Semitones -> Hz
f n = pitchStandard * (2**(1.0/12.0))**n

note :: Semitones -> Seconds -> [Pulse]
note n duration = freq (f n) duration

freq :: Hz -> Seconds -> [Pulse]
freq hz duration = map (* volume) $ map sin $ map (*step) [0.0 .. sampleRate * duration]
    where step = (hz *2 *pi)/ sampleRate

volume :: Float
volume = 0.5

--- middle C : 262,265,264 hz




wave :: [Pulse]
-- wave = concat [freq 440.0 1.0, freq 265.0 1.0]
--wave = concat [freq (pitchStandard + i * 100.0) duration | i<-[0..10]]
--     where 
--         duration = 1.0

wave = concat [note i duration | i<-[0..10]]
     where 
         duration = 1.0

--- SCALE 
wave2 :: [Pulse]
wave2  = concat [note 0 duration
                , note 2 duration
                , note 4 duration
                , note 5 duration
                , note 7 duration
                , note 9 duration
                , note 11 duration
                , note 12 duration
                ]
         where
              duration = 0.5

-- sin x = 0<=x<=2*pi



save :: FilePath -> IO ()
save filePath = B.writeFile filePath $ B.toLazyByteString $ fold $ map B.floatLE wave 

save2 :: FilePath ->IO()
save2 filePath = B.writeFile filePath $ B.toLazyByteString $ fold $ map B.floatLE wave2


-- <- means ignoring result 
play:: IO ()
play = do
  save outputFilePath
  _ <- runCommand $ printf "ffplay -showmode 1 -f f32le -ar %f %s" sampleRate outputFilePath
  return()
play2 :: IO()
play2 = do   
  save2 outputFileScale
  _ <- runCommand $ printf "ffplay -showmode 1 -f f32le -ar %f %s" sampleRate outputFileScale
  return()
  
-- can use Haskell as a shell script language 


