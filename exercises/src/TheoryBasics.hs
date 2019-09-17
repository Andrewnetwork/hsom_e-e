{--
    TheoryBasics.hs
    Andrew Ribeiro 
    September 2019

    These are my notes for a review of music theory.
    https://www.musictheory.net
--}
module TheoryBasics where
import Euterpea

data StaffLine = L1 | L2 | L3 | L4 | L5 deriving(Enum)
data StaffSpace = S1 | S2 | S3 | S4 deriving(Enum)
data StaffPosition = Line StaffLine | Space StaffSpace 
type StaffNote = (StaffPosition, Dur)
data Clef = Treble | Bass
data Staff = Staff Clef [[StaffNote]] 

tst :: Staff
tst = Staff Treble [[(Space S1,qn),(Space S2,qn),(Space S3,qn)],[(Line L1,qn),(Line L2,qn),(Line L3,qn)]]

notePitch :: Clef -> StaffPosition -> PitchClass 
notePitch Treble (Line l) = [E,G,B,D,F] !! fromEnum l
notePitch Treble (Space s) = [F,A,C,E] !! fromEnum s

staffNote :: Octave -> Clef -> StaffNote -> Music Pitch
staffNote oct clef (staffPos, dur) = note dur (notePitch clef staffPos, oct)

makeMusic :: Staff -> Music Pitch
makeMusic (Staff Treble []) = rest 0
makeMusic (Staff Treble (x:xs)) = chord (staffNote 4 Treble <$> x) :+: makeMusic (Staff Treble xs)

-- >>> play $ makeMusic tst

musicSeq :: [Music a] -> Music a
musicSeq = foldl (:+:) (rest 0)

-- 
