{--
    TheoryBasics.hs
    Andrew Ribeiro 
    September 2019

    These are my notes for a review of music theory.
    https://www.musictheory.net
--}
{-# LANGUAGE TupleSections #-}
module TheoryBasics where
import Euterpea
import Lib 

data StaffLine = L1 | L2 | L3 | L4 | L5 deriving(Enum)
data StaffSpace = S1 | S2 | S3 | S4 deriving(Enum)
data StaffPosition = Line StaffLine | Space StaffSpace 
type StaffNote = (StaffPosition, Dur)
data Clef = Treble | Bass
data Staff = Staff Clef [[StaffNote]] 
type NoteConstructor = Octave -> Dur -> Music Pitch

tst :: Staff
tst = Staff Treble [[(Space S1,hn),(Space S2,hn),(Space S3,hn)],[(Line L1,hn),(Line L2,hn),(Line L3,hn)]]

notePitch :: Clef -> StaffPosition -> PitchClass 
notePitch Treble (Line l) = [E,G,B,D,F] !! fromEnum l
notePitch Treble (Space s) = [F,A,C,E] !! fromEnum s

staffNote :: Octave -> Clef -> StaffNote -> Music Pitch
staffNote oct clef (staffPos, dur) = note dur (notePitch clef staffPos, oct)

makeMusic :: Staff -> Music Pitch
makeMusic (Staff Treble []) = rest 0
makeMusic (Staff Treble (x:xs)) = chord (staffNote 4 Treble <$> x) :+: makeMusic (Staff Treble xs)


(>*<) :: [a->b] -> [a] -> [b]
fs >*< (x:xs) = (fs <*> pure x) ++ (fs >*< xs)
_ >*< [] = []
infixl 4 >*<

-- >>> play $ makeMusic tst
-- >>> play $ musicSeq $ uncurry <$> [c, d, e, f, g, a, b, bs] <*> [(2::Int, en),(4::Int, en)]
-- >>> play $ musicSeq $ uncurry <$> [c, d, e, f, g, a, b, bs] >*< [(2::Int, en),(4::Int, en)]
-- >>> play $ musicSeq $ [ n u | n <- uncurry <$> [c, d, e, f, g, a, b, bs], u <- [(2::Int, en),(4::Int, en)] ]
-- >>> play $ musicSeq $ (uncurry <$> [c, d, e, f, g, a, b]) >*< ((\x->(x,sn)) <$> [0..6])
-- [succ, pred] >*< [1,2,3] 

createScale :: [Octave] -> Dur -> Music Pitch
createScale octs dur = musicSeq $ (uncurry <$> [c, d, e, f, g, a, b]) >*< ((, dur) <$> octs) 
-- play $ createScale [0..8] qn :=: createScale [0..8] en :=: createScale [0..8] sn
-- play $ createScale [8,7..0] sn
-- 
notePlayer :: (Functor f1, Functor f2) =>
              (f1 ((a1, b) -> c) -> f2 (t1, t2) -> [Music a2])
              -> f1 (a1 -> b -> c) 
              -> f2 t1 -> t2 -> Music a2 
notePlayer playOp notes octs dur = musicSeq $ (uncurry <$> notes) `playOp` ((, dur) <$> octs) 

noteSplitter :: [Octave -> Dur -> Music Pitch] -> [Octave -> Dur -> Music Pitch]
noteSplitter fs = fs >>= \f -> [\oct dur -> f oct (dur/2), \oct dur -> f oct (dur/2)]


splitOverOctaves :: [NoteConstructor] -> Dur -> [[Octave]] -> Music Pitch
splitOverOctaves notes dur octs = snd $ foldl (\(n,m) o -> (noteSplitter n, notePlayer (<*>) n o dur :=: m)) (notes,rest 0) octs 

twinkleRedux = play $ splitOverOctaves [c,c,g,g,a,a,g,f,f,e,e,d,d,c,g,g,f,f,e,e,d,g,g,f,f,e,e,d,c,c,g,g,a,a,g,f,f,e,e,d,d,c] 
                      hn [[6,3,2],[3,1,2],[4,1,4],[1,2,3]]

w1 = splitOverOctaves [e,ef,d,df,es,ff,af,cs] wn [[1,1,2],[3,3,4],[5,5,6],[7,7,8]]
w2 = splitOverOctaves [e,ef,d,df,es,ff,af,cs] hn [[1,1,2],[3,3,4],[5,5,6],[7,7,8]]
w3 = splitOverOctaves [e,ef,d,df,es,ff,af,cs] qn [[1,1,2],[3,3,4],[5,5,6],[7,7,8]]
w4 = splitOverOctaves [e,ef,d,df,es,ff,af,cs] en [[1,1,2],[3,3,4],[5,5,6],[7,7,8]]
w5 = splitOverOctaves [e,ef,d,df,es,ff,af,cs] sn [[1,1,2],[3,3,4],[5,5,6],[7,7,8]]
timesRunningOut = play $ w1 :+: w2 :+: w3 :+: w4 :+: w5 


wt ls = splitOverOctaves [c,cs,df,ds,cf,c,es,df,cf] qn ls
wt2 ls = splitOverOctaves [c,c] qn ls
theRedDance = play $ (Modify (Instrument PercussiveOrgan) $ (wt [[5,5,6]]) :+: (wt [[5,5,6],[3,3,4]]) :+: (wt [[5,5,6],[3,3,4],[1,1,2]] ) :+: (wt [[3,3,4],[5,5,6],[1,1,2]]) :+: (wt [[1,1,2],[3,3,4],[5,5,6]]) 
                     :+: (wt [[1,1,2],[5,5,6],[3,3,4]]) :+: (wt [[7,7,8],[3,3,4],[5,5,6],[1,1,2]])) :+: 
                      (Modify (Instrument Gunshot)) (c 4 qn :+: c 4 qn)

-- >>> play $ splitOverOctaves [c,e,g,gf,b] hn [[2,4,2],[4,2,4],[2,4,6],[2,5,3]]
-- >>> play $ splitOverOctaves [c,c,g,g,a,a,g,f,f,e,e,d,d,c,g,g,f,f,e,e,d,g,g,f,f,e,e,d,c,c,g,g,a,a,g,f,f,e,e,d,d,c] hn [[6,3,2],[3,1,2],[4,1,4],[1,2,3]]
-- >>> play $ splitOverOctaves [a,b,c,d,e,f,g,g] hn [[6,3,2],[3,1,2],[4,1,4],[1,2,3]]


-- let m1 = noteSplitter notes
--     m2 = noteSplitter m1
--     m3 = noteSplitter m2 
-- in chord [(notePlayer m1 ls1 hn),(notePlayer m2 ls2 hn),(notePlayer m3 ls3 hn)]



-- noteSplitter ([c,d,b,a],wn) 

-- noteSplitter' :: [Octave -> Dur -> Music Pitch] -> [Octave -> Dur -> Music Pitch]
-- noteSplitter' noteConstructors = noteConstructors >>= replicate 2 

-- tst3 octs =  Modify (Instrument RockOrgan) $  notePlayer [c, c, d, d, b, b, a, a] octs sn 
-- tst4 octs = Modify (Instrument TubularBells) $ notePlayer [c, d, b, a] octs en 
-- tst5 = tst3 (replicate 8 1) :=: tst3 (replicate 8 7) :=: tst4 [4,4,5,5,4,4,5,5]
-- play $ musicSeq $ (replicate 2 tst5) ++ [tst4 [3,4,7]]

-- tst3' :: [Octave] -> Music Pitch
-- tst3' = Modify (Instrument RockOrgan) . notePlayer' (noteSplitter [c, d, b, a] en) 
-- >>> play $ tst3 [2,3,4]
-- >>> play $ tst3' [2,3,4]

--test8 = 


-- >>> play $ tst4 [2,3,4]

-- play $ tst3 [2,3,4] :=: tst4 [2,3,4]

--

-- play $ Modify (Instrument Gunshot) $ musicSeq $ (replicate 2 tst5) 
-- play $ Modify (Instrument RockOrgan) $ musicSeq $ (replicate 2 tst5)

-- tst6 =  notePlayer [c, d, e, f, g] [1,1] wn 
-- tst7 oct n = musicSeq $ concat $ replicate n [c oct wn, c oct qn, c oct wn, c oct qn, e oct wn, e oct qn, es oct wn, e oct qn]

-- >>> play $ (Modify (Instrument RockOrgan) $ tst7 2 2) 

drumLine n = musicSeq $ concat $ replicate n [perc AcousticSnare en, perc AcousticSnare en, perc AcousticSnare en, perc SplashCymbal en, perc ClosedHiHat en, perc SplashCymbal en, perc ClosedHiHat en, perc SplashCymbal en]

-- >>> play $ Modify (Tempo 2) (drumLine 5)