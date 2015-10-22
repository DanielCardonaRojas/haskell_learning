{-# LANGUAGE Arrows #-}

import Euterpea
import Control.Arrow ((>>>),(<<<),arr)

pulseTable :: Table
pulseTable = tableLinearN 4096 1 [(0.15, 1), (0, -1), (0.85, -1)]

pulseOsc :: AudSF Double Double
pulseOsc = osc squareTable 0

squareTable :: Table
squareTable = tableLinearN 4096 1 [(0.5, 1), (0, -1), (0.5, -1)]

octaverOsc :: AudSF Double Double
octaverOsc = proc f -> do
  note   <- pulseOsc -< f
  octave <- pulseOsc -< 2*f
  outA -< (note + octave) / 2

--puretone table
tab1 :: Table
tab1 = tableSinesN 4096 [1]

tab2 :: Table
tab2 = tableSinesN 4096 [1.0, 0.5, 0.33]

-- | This produces a signal funcion for vibrato
vibrato :: Double -> Double -> AudSF Double Double
vibrato vfrq dep = proc afrq -> do
	 vib <- osc tab1 0 -< vfrq
	 aud <- osc tab2 0 -< afrq + vib * dep
	 outA -< aud

s5 = constA 300 >>> vibrato 5 20

organBaseSound = outFile "OrganBaseSound.wav" 3 (s5)

main = organBaseSound