module Lib where
import Euterpea

someFunc :: IO ()
someFunc = putStrLn "someFunc"

mel1 :: Octave -> Music Pitch
mel1 x = a x qn :+: b x qn :+: c x qn
-- >>> play $ mel1 2

hNote :: Dur -> Pitch -> Music Pitch 
hNote d p = note d p :=: note d (trans (-3) p)

-- >>> play $ hNote wn (A,4)

hList :: Dur -> [Pitch] -> Music Pitch
hList d [] = rest 0 
hList d (p:ps) = hNote d p :+: hList d ps 

mel2 :: Int -> Dur -> Octave -> Music Pitch
mel2 n dur oct = hList dur $ take n (cycle [(A,oct),(B,oct),(C,oct),(D,oct)])
-- >>> play $ (mel2 20 sn 4) :=: (mel2 20 qn 3) :=: (mel2 20 wn 2) :=: (mel2 20 tn 1)

mel3 0 = hNote qn (A,0)
mel3 n = hNote tn (A,mod n 5) :+: hNote qn (C,mod n 10) :+: mel3 (pred n)

-- >>> play $ mel3 100

thn :: Dur
thn = 1/100

-- play $ Prim (Note qn ())
a440m :: Music Pitch
a440m = Prim (Note en (A,4))

mod_a440m :: InstrumentName -> Music Pitch 
mod_a440m instrument = Modify (Instrument instrument) a440m

musicSeq :: [Music a] -> Music a
musicSeq = foldl (:+:) (rest 0)

-- Problem: Given a list of instruments, play a440m on each of them. 
playList :: [InstrumentName] -> Music Pitch
playList = foldr ((:+:) . mod_a440m) (rest 0) 

-- >>> play $ playList [FretlessBass,Glockenspiel,Trumpet]
-- >>> play $ playList (enumFrom(toEnum 0))
-- >>> play $ transpose 2 (mel2 10 sn 4)
-- >>> play $ transpose 4 (mel2 10 sn 4) :+: transpose 3 (mel2 10 sn 4) :+: transpose 2 (mel2 10 sn 4) :+: transpose 1 (mel2 10 sn 4)
moteif1 :: [Int] -> Music a -> Music a
moteif1 transLs music = musicSeq $ map (`transpose` music) transLs

moteif2 :: [Dur] -> Music a -> Music a 
moteif2 transLs music = musicSeq $ map (`tempo` music) transLs


-- play $ moteif1 [5,4..0] (mel2 50 sn 4) :+: moteif1 [1..5] (mel2 50 sn 4)
-- play $ moteif1 [0..5] t251
-- play $ moteif2 [en,] (mel1 2)
-- play $ transpose 1 (mel2 10 sn 4)

t251 :: Music Pitch
t251 = let dMinor = d 4 wn :=: f 4 wn :=: a 4 wn
           gMajor = g 4 wn :=: b 4 wn :=: d 5 wn
           cMajor = c 4 bn :=: e 4 bn :=: g 4 bn
       in dMinor :+: gMajor :+: cMajor
-- >>> play t251

-- Exercise 2.1 
twoFiveOne :: Pitch -> Dur -> Music Pitch
twoFiveOne key dur = let r' = note dur key
                         d' = transpose 7 r'
                         m' = transpose 2 r'
                         minor = m' :=: (transpose 3 m') :=: (transpose 7 m')
                         dominant = d' :=: (transpose 4 d') :=: (transpose 7 d')
                         root = r' :=: (transpose 4 r') :=: (transpose 7 r')
                      in minor :+: dominant :+: root
-- play $ twoFiveOne (C, 4) wn
-- play $ (twoFiveOne (C, 4) wn) :=: t251
-- twoFiveOne (C, 4) wn == t251.
-- play $ (twoFiveOne (C, 4) wn) :=: t251

majorChord :: Music Pitch -> Music Pitch
majorChord m = m :=: transpose 4 m :=: transpose 7 m

minorChord :: Music Pitch -> Music Pitch
minorChord m = m :=: transpose 3 m :=: transpose 7 m


-- >>> play $ (c 4 hn :=: e 4 hn :=: g 4 hn)
-- >>> play $ majorChord (c 4 hn)
-- >>> play $ (e 4 hn :=: g 4 hn :=: b 4 hn)
-- >>> play $ minorChord (e 4 hn) :=: (e 4 hn :=: g 4 hn :=: b 4 hn)

twinkleTwinkleTreble :: Octave -> Music Pitch
twinkleTwinkleTreble oct = let m1 = c oct qn :+: c oct qn :+: g oct qn :+: g oct qn 
                               m2 = a oct qn :+: a oct qn :+: g oct hn  
                               m3 = f oct qn :+: f oct qn :+: e oct qn :+: e oct qn
                               m4 = d oct qn :+: d oct qn :+: c oct hn
                               m5 = g oct qn :+: g oct qn :+: f oct qn :+: f oct qn    
                               m6 = e oct qn :+: e oct qn :+: d oct hn       
                               m7 = m5
                               m8 = m6
                               m9 = m1 
                               m10 = m2
                               m11 = m3
                               m12 = m4
                           in  m1 :+: m2 :+: m3 :+: m4 :+: m5 :+: m6 :+: m7 :+: m8 :+: m9 :+: m10 :+: m11 :+: m12


twinkleTwinkleBass :: Octave -> Music Pitch
twinkleTwinkleBass oct = let m1 = majorChord (c oct hn) :+: minorChord (e oct hn) 
                             m2 = (c oct hn) :=: (f oct hn) :=: (a oct hn)
                         in  m1 :+: m2

-- >>> play $ twinkleTwinkleTreble 4
-- >>> play $ twinkleTwinkleBass 4
-- >>> play $ (twinkleTwinkleTreble 4) :=: (twinkleTwinkleBass 3)
-- >>> play $ twinkleTwinkleTreble 5 :=: twinkleTwinkleTreble 4 :=: twinkleTwinkleTreble 3 :=: twinkleTwinkleTreble 2 :=: twinkleTwinkleTreble 1
-- >>> play $ twinkleTwinkleTreble (-1) :=: twinkleTwinkleTreble 8
-- >>> play $ Modify (Instrument ChoirAahs) (twinkleTwinkleTreble 5)
-- >>> play $ Modify (Tempo 2) (twinkleTwinkleTreble 5)
-- >>> play $ Modify (KeySig G Major) (twinkleTwinkleTreble 5)
-- >>> play $ twinkleTwinkleBass 3
-- >>> play $ majorChord (f 4 hn)
-- >>> play $ (c 4 hn) :=: (f 4 hn) :=: (a 4 hn)