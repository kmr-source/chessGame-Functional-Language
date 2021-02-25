import Control.Concurrent 
import Euterpea
--http://hackage.haskell.org//base-4.12.0.0/docs/Control-Concurrent.html
-- credit: https://stackoverflow.com/questions/54865136/start-euterpea-music-in-main-function-together-with-gameworld-in-haskell
-- mian function used:  https://wiki.haskell.org/Introduction_to_Haskell_IO/Actions
--main = do musicThreadId <- forkIO $ Euterpea.play $ Euterpea.line [af 4 dqn :=: cf 4 dqn :=: ef 4 dqn] 
melody :: Music Pitch
melody = line [c 5 qn, g 5 qn, g 5 qn, c 5 qn, g 5 qn, g 4 hn, c 4 qn, g 5 qn, g 5 qn, c 3 qn, g 3 qn, g 5 hn]
chords :: Music Pitch
chords = chord [ c 3 wn, e 3 wn, g 3 wn] :+:
         chord [ c 3 hn, f 3 hn, a 3 hn] :+:
         chord [ e 3 hn, g 4 hn, c 3 hn] :+:
		 chord [ c 3 wn, e 3 wn, g 3 wn] :+:
         chord [ c 3 hn, f 3 hn, a 3 hn] :+:
         chord [ c 3 hn, d 4 hn, e 3 hn] :+:
		 chord [ c 3 wn, e 3 wn, g 3 wn] :+:
         chord [ c 3 hn, f 3 hn, a 3 hn] :+:
         chord [ e 3 hn, g 4 hn, c 3 hn] :+:
		 chord [ c 3 wn, e 3 wn, g 3 wn] :+:
         chord [ c 3 hn, f 3 hn, a 3 hn] :+:
         chord [ c 3 hn, d 4 hn, e 3 hn] :+:
		 chord [ c 3 wn, e 3 wn, g 3 wn] :+:
         chord [ c 3 hn, f 3 hn, a 3 hn] :+:
         chord [ e 3 hn, g 4 hn, c 3 hn] :+:
		 chord [ c 3 wn, e 3 wn, g 3 wn] :+:
         chord [ c 3 hn, f 3 hn, a 3 hn] :+:
         chord [ c 3 hn, d 4 hn, e 3 hn] :+:
		 chord [ c 3 wn, e 3 wn, g 3 wn] :+:
         chord [ c 3 hn, f 3 hn, a 3 hn] :+:
         chord [ e 3 hn, g 4 hn, c 3 hn] :+:
		 chord [ c 3 wn, e 3 wn, g 3 wn] :+:
         chord [ c 3 hn, f 3 hn, a 3 hn] :+:
         chord [ c 3 hn, d 4 hn, e 3 hn] :+:
		 chord [ c 3 hn, f 3 hn, a 3 hn] :+:
         chord [ e 3 hn, g 4 qn, c 3 hn] :+:
		 chord [ c 3 wn, e 3 qn, g 3 wn] :+:
         chord [ c 3 hn, f 3 qn, a 3 hn] :+:
         chord [ c 3 qn, d 4 hn, e 3 wn] 
twinkle :: Music Pitch
twinkle = melody :=: chords :=: melody

melody2 :: Music Pitch
melody2 = Euterpea.line [af 4 dqn :=: cf 4 dqn]

musicBeginning :: Music Pitch
musicBeginning = line [a 5 qn, b 5 qn, c 5 qn, a 5 qn, b 5 qn, c 4 hn, a 4 qn, e 5 qn, g 5 qn, b 3 qn, c 3 wn]

muscEnd :: Music Pitch
muscEnd = line [e 5 qn, e 5 qn, e 5 qn, d 5 qn, c 5 qn, c 6 hn]

--musicThread :: forkIO

main :: IO()
main = do playDev 0 (twinkle)

main2 :: IO ()
main2 = do
    putStrLn "Enter yes or no"
    do
        line1 <- getLine                                -- line1 :: String
        do 
	      if (line1 == "yes")
		    then play musicBeginning
		  else
		     do putStrLn "Game has ended"
		        play muscEnd