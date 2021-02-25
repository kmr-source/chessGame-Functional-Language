-- Sin waves for music 
-- credit: https://www.youtube.com/watch?v=FYTZkE5BZ-0&t=91s

import qualified Data.ByteString.Lazy as B
import qualified Data.ByteString.Builder as B
import Data.Foldable 
import System.Process
import Data.List
-- formatting string 
import Text.Printf

outputFilePath:: FilePath
outputFilePath = "output.bin"

outputFileScale:: FilePath
outputFileScale = "scale.bin"

outputFileScale2:: FilePath
outputFileScale2 = "scale2.bin"

type Pulse = Float
type Seconds = Float
type Samples = Float
type Hz = Float
type Semitones = Float
type Beats = Float

sampleRate :: Samples
sampleRate = 48000.00

pitchStandard :: Hz
pitchStandard = 440.0

bpm :: Beats
bpm = 120.0

beatDuration :: Seconds
beatDuration = 60.0 / bpm


f :: Semitones -> Hz
f n = pitchStandard * (2**(1.0/12.0))**n


note :: Semitones -> Beats -> [Pulse]
note n beats = freq (f n) (beats * beatDuration)

freq :: Hz -> Seconds -> [Pulse]
freq hz duration = map (* volume) $ zipWith3 (\x y z -> x*y*z) release attack output
    where step = (hz *2 *pi)/ sampleRate

          attack :: [Pulse]
          attack = map (min 1.0) [0.0,0.001 ..]
          
          release :: [Pulse]
          release = reverse $ take (length output) attack

          output :: [Pulse]
          output =  map sin $ map (*step) [0.0 .. sampleRate * duration]

volume :: Float
volume = 0.5

--- middle C : 262,265,264 hz




wave :: [Pulse]
-- wave = concat [freq 440.0 1.0, freq 265.0 1.0]
--wave = concat [freq (pitchStandard + i * 100.0) duration | i<-[0..10]]
--     where 
--         duration = 1.0

wave = concat [note i 1.0 | i<-[0..10]]
     where 
         duration = 1.0

--- SCALE 
wave2 :: [Pulse]
wave2  = concat [note 0 1.0
                , note 2 1.0
                , note 4 1.0
                , note 5 1.0
                , note 7 1.0
                , note 9 1.0
                , note 11 1.0
                , note 12 1.0
                ]

-- note 0 = c
-- note 2 = d
-- note 4 = e 
-- note 5 = f
-- note 7 = g
-- note 9 = a
-- note 11 = b
-- note 12 = c
wave3 :: [Pulse]
wave3  = concat [note 0 1.0
                , note 1 1.0
                , note 1 1.0
                , note 1 1.0
                , note 1 1.0
                , note 2 1.0
                , note 2 1.0
                , note 2 1.0
                , note 3 1.0
                , note 4 1.0
                , note 5 1.0
                , note 6 1.0
                , note 7 1.0
                , note 8 1.0
                , note 9 1.0
                , note 10 1.0
                , note 11 1.0
                , note 12 1.0
                ]
      
-- sin x = 0<=x<=2*pi



save :: FilePath -> IO ()
save filePath = B.writeFile filePath $ B.toLazyByteString $ fold $ map B.floatLE wave 

save2 :: FilePath ->IO()
save2 filePath = B.writeFile filePath $ B.toLazyByteString $ fold $ map B.floatLE wave2

save3 :: FilePath ->IO()
save3 filePath = B.writeFile filePath $ B.toLazyByteString $ fold $ map B.floatLE wave3

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
play3:: IO()
play3 = do   
  save3 outputFileScale2
  _ <- runCommand $ printf "ffplay -showmode 1 -f f32le -ar %f %s" sampleRate outputFileScale2
  return()  
-- can use Haskell as a shell script language 


