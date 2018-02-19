{-# LANGUAGE TemplateHaskell #-}

module Music where

import Control.Monad
import Test.QuickCheck

import Music.Ring
import Music.Util


-- http://www.mta.ca/pc-set/pc-set_new/pages/introduction/toc.html
-- https://youtu.be/P6DvIfTJhx8?t=437
-- http://reasonablypolymorphic.com//blog/modeling-music
-- https://fgiesen.wordpress.com/2015/09/24/intervals-in-modular-arithmetic/
-- http://www.musictheory.net/calculators/interval


-- -----------------------------------------------------------------------------
-- Diatonic pitch class names using Letters

-- The 7 distinct letters
data Letter = C_ | D_ | E_ | F_ | G_ | A_ | B_
              deriving (Eq, Show, Enum, Bounded)
                       
instance Arbitrary Letter where
    arbitrary = arbitraryBoundedEnum
                
-- these are the 7 diatonic pitch classes
dpc = map mkX [0,2,4,5,7,9,11]

-- mapping letters to pitch classes (7 -> 12)
l_x :: Letter -> X
l_x = (dpc !!) . fromEnum

-- any letter maps to a pitch class position within the X ring
prop_l_x_bounds l = let a = unX (minBound::X)
                        b = unX (maxBound::X)
                        x = unX (l_x l)
                    in x >= a && x <= b

-- any letter must map to a diatonic tone, not a chromatic tone
prop_l_x_diatonic = (`elem` dpc) . l_x





-- -----------------------------------------------------------------------------
-- the common accidentals (9 elements)

-- these are numerically ordered
data Accidental = QuadrupleFlat | TripleFlat | DoubleFlat | Flat
                | Natural
                | Sharp | DoubleSharp | TripleSharp | QuadrupleSharp
                  deriving (Eq, Show, Enum, Bounded)
                           
instance Arbitrary Accidental where
    arbitrary = arbitraryBoundedEnum
                           
-- mapping accidentals to pitch class differences
a_n :: Accidental -> Int
a_n a = fromEnum a - fromEnum Natural

-- any accidental moves a pitch at most 4 semitones
prop_a_n_bounds = (< 5) . abs . a_n





-- -----------------------------------------------------------------------------
-- Pitch classes, named using Letters and Accidentals

-- a pitch class is a letter plus accidental (49 elements)
data PitchClass = PC Letter Accidental
                  deriving (Eq, Show)
                           
instance Arbitrary PitchClass where
    arbitrary = liftM2 PC arbitrary arbitrary

-- extracting letter from pitch class (49 -> 7)
pc_l :: PitchClass -> Letter
pc_l (PC l _) = l

-- extracting accidental from pitch class (49 -> 7)
pc_a :: PitchClass -> Accidental
pc_a (PC _ a) = a

-- mapping pitch class to underlying set (49 -> 12)
pc_x :: PitchClass -> X
pc_x (PC l a) = mkX $ mod d m
    where
      d = unX (l_x l) + a_n a
      m = unX (maxBound :: X) + 1

-- a Natural PitchClass maps to a diatonic pitch class
prop_pc_natural pc = pc_a pc == Natural ==>
                     pc_x pc `elem` dpc




                

-- -----------------------------------------------------------------------------
-- the 11 standard octaves
-- these are numerically ordered

-- data Octave = Ox | O0 | O1 | O2 | O3 | O4 | O5 | O6 | O7 | O8 | O9
--              deriving (Eq, Ord, Show, Enum, Bounded)
                              
-- a pitch is a letter, accidental and octave
-- data Pitch = P Letter Accidental Octave
--             deriving (Eq, Ord, Show)










                      
-- -----------------------------------------------------------------------------
-- The 17 common pitch class labels

data PCLabel = C | C'
             | Db | D | D'
             | Eb | E
             | F | F'
             | Gb | G | G'
             | Ab | A | A'
             | Bb | B
               deriving (Eq, Show, Enum, Bounded)

instance Arbitrary PCLabel where
    arbitrary = arbitraryBoundedEnum

-- mapping label names onto pitch classes (17 -> 49)
pcl_pc :: PCLabel -> PitchClass
pcl_pc C  = PC C_ Natural
pcl_pc C' = PC C_ Sharp
pcl_pc Db = PC D_ Flat
pcl_pc D  = PC D_ Natural
pcl_pc D' = PC D_ Sharp
pcl_pc Eb = PC E_ Flat
pcl_pc E  = PC E_ Natural
pcl_pc F  = PC F_ Natural
pcl_pc F' = PC F_ Sharp
pcl_pc Gb = PC G_ Flat
pcl_pc G  = PC G_ Natural
pcl_pc G' = PC G_ Sharp
pcl_pc Ab = PC A_ Flat
pcl_pc A  = PC A_ Natural
pcl_pc A' = PC A_ Sharp
pcl_pc Bb = PC B_ Flat
pcl_pc B  = PC B_ Natural

-- pc labels only name a few types of accidentals
prop_pcl_pc_accidentals = (`elem` [Flat, Natural, Sharp]) . pc_a . pcl_pc

-- Mapping label names onto pitch classes (17 -> 12)
pcl_x :: PCLabel -> X
pcl_x = pc_x . pcl_pc

-- mapping pitch classes to enharmonic label names
x_pcl :: X -> [PCLabel]
x_pcl x = filter ((== x) . pcl_x) enums

-- any pitch class has at most two enharmonic labels
prop_x_pcl_enharmonic_count = (<= 2) . length . x_pcl

-- Mapping label names onto letters (17 -> 7)
pcl_l :: PCLabel -> Letter
pcl_l = pc_l . pcl_pc

-- mapping letters to sets of label names
l_pcl :: Letter -> [PCLabel]
l_pcl x = filter ((== x) . pcl_l) enums

-- any letter has at most three common pitch class labels
prop_l_pcl_label_count l = (<= 3) . length . l_pcl

-- Mapping label names onto accidentals (17 -> 7)
pcl_a :: PCLabel -> Accidental
pcl_a = pc_a . pcl_pc

-- mapping accidentals to sets of label names
a_pcl :: Accidental -> [PCLabel]
a_pcl x = filter ((== x) . pcl_a) enums

-- check label counts for each accidental
prop_a_pcl_label_count a = let l = length (a_pcl a) in
                           case a of
                             Flat           -> l == 5
                             Natural        -> l == 7
                             Sharp          -> l == 5
                             otherwise      -> l == 0




          
-- -----------------------------------------------------------------------------
-- pitch class intervals

-- pitch difference class between two pitch classes (2401 -> 12)

-- ascending interval size between two pitchclasses
pp_y :: PitchClass -> PitchClass -> Int
pp_y p1 p2 = mod d m
    where
      d = f p2 - f p1
      m = unY (maxBound :: Y) + 1
      f = unX . pc_x

-- calculate pitch class interval in three ways
pp_asc_y = pp_y
pp_dsc_y = flip pp_y 
pp_min_y p1 p2 = min (pp_asc_y p1 p2) (pp_dsc_y p1 p2)

-- asc and dsc intervals add up to 0 or 12
prop_pp_y_sum p1 p2 = let n = pp_asc_y p1 p2 + pp_dsc_y p1 p2
                      in if pc_x p1 == pc_x p2
                         then n == 0
                         else n == 12

-- min is <= both other measures
prop_pp_y_min p1 p2 = let m = pp_min_y p1 p2
                          a = pp_asc_y p1 p2
                          d = pp_dsc_y p1 p2
                      in m <= a && m <= d




--------------------------------------------------------------------------------
-- the sizes of simple Interval (15 elements)

-- these are numerically ordered
data DiatonicNumber = Unison
                    | Second
                    | Third
                    | Fourth
                    | Fifth
                    | Sixth
                    | Seventh
                    | Octave
                    | Ninth
                    | Tenth
                    | Eleventh
                    | Twelfth
                    | Thirteenth
                    | Fourteenth
                    | Fifteenth
                      deriving (Eq, Ord, Show, Enum, Bounded)

instance Arbitrary DiatonicNumber where
    arbitrary = arbitraryBoundedEnum

-- mapping a diatonic number to a pitch difference class (15 -> 12)
-- this is the natural/major scale. the differences are measured from
-- C Major here
dn_y :: DiatonicNumber -> Y
dn_y = mkY . unX . (dpc !!) . (`mod` (length dpc)) . fromEnum

-- Diatonic pitch differences map onto diatonic pitch classes
-- (measured from C Major)
prop_dn_y_diatonic = (`elem` dpc) . mkX . unY . dn_y

                        

--------------------------------------------------------------------------------
-- the qualities of an Interval (15 elements)

data Quality = SextuplyDiminished
             | QuintuplyDiminished
             | QuadruplyDiminished
             | TriplyDiminished
             | DoublyDiminished
             | Diminished
             | Minor
             | Perfect
             | Major
             | Augmented
             | DoublyAugmented
             | TriplyAugmented
             | QuadruplyAugmented
             | QuintuplyAugmented
             | SextuplyAugmented
               deriving (Eq, Show, Enum, Bounded)

instance Arbitrary Quality where
    arbitrary = arbitraryBoundedEnum

                        

--------------------------------------------------------------------------------
-- an Interval has a quality and a size (225 elements)

data Interval = I Quality DiatonicNumber
                deriving (Eq, Show)

instance Arbitrary Interval where
    arbitrary = liftM2 I arbitrary arbitrary

-- diatonic number between two pitch classes (2401 -> 8) this relies
-- on the first 7 items of DiatonicNumber being the same as the
-- elements of Letter
pp_dn :: PitchClass -> PitchClass -> DiatonicNumber
pp_dn p1 p2 = toEnum $ mod d m
    where
      d = f p2 - f p1
      m = fromEnum (maxBound::Letter) + 1
      f = fromEnum . pc_l

-- pitch classes with same letter form a unison
prop_pp_dn_unison p1 p2 = pc_l p1 == pc_l p2 ==>
                          pp_dn p1 p2 `elem` [Unison, Octave]

pp_q :: PitchClass -> PitchClass -> Quality
pp_q p1 p2 = classify difference
    where
      -- calculate the general interval size
      i = pp_dn p1 p2

      -- calculate the pitch difference from the standard interval
      difference = actual - expected
          where
            actual = mod d m
            expected = y i
            d = x p2 - x p1
            m = unX (maxBound :: X) + 1
            x = unX . pc_x
            y = unY . dn_y

      -- based on the general interval type, select a function to map
      -- the pitch difference to a specific interval quality
      classify = q i
          where
            -- mapping a diatonic number to a quality mapper
            q :: DiatonicNumber -> (Int -> Quality)
            q Unison  = q1
            q Second  = q2
            q Third   = q2
            q Fourth  = q1
            q Fifth   = q1
            q Sixth   = q2
            q Seventh = q2
            q Octave  = q1

            -- mapping a pitch difference to a quality
            q1 :: Int -> Quality
            q1 d = case (mod d 12) of
                     7  -> QuintuplyDiminished
                     8  -> QuadruplyDiminished
                     9  -> TriplyDiminished
                     10 -> DoublyDiminished
                     11 -> Diminished
                     0  -> Perfect
                     1  -> Augmented
                     2  -> DoublyAugmented
                     3  -> TriplyAugmented
                     4  -> QuadruplyAugmented
                     5  -> QuintuplyAugmented
                     6  -> SextuplyAugmented
                     _  -> error $ "q1 " ++ (show d)
              
            -- mapping a pitch difference to a quality
            q2 :: Int -> Quality
            q2 d = case (mod d 12) of
                     7  -> QuadruplyDiminished
                     8  -> TriplyDiminished
                     9  -> DoublyDiminished
                     10 -> Diminished
                     11 -> Minor
                     0  -> Major
                     1  -> Augmented
                     2  -> DoublyAugmented
                     3  -> TriplyAugmented
                     4  -> QuadruplyAugmented
                     5  -> QuintuplyAugmented
                     6  -> SextuplyAugmented
                     _  -> error $ "q2 " ++ (show d)


interval :: PitchClass -> PitchClass -> Interval
interval p1 p2 = I (pp_q p1 p2) (pp_dn p1 p2)






          


                 
-- https://en.wikipedia.org/wiki/Interval_(music)#Main_intervals
-- https://en.wikipedia.org/wiki/Interval_(music)#Main_compound_intervals

-- the primary diatonic intervals (52 elements)
data DInterval = I_P1 | I_d2
               | I_m2 | I_A1
               | I_M2 | I_d3
               | I_m3 | I_A2
               | I_M3 | I_d4
               | I_P4 | I_A3
               | I_d5 | I_A4
               | I_P5 | I_d6
               | I_m6 | I_A5
               | I_M6 | I_d7
               | I_m7 | I_A6
               | I_M7 | I_d8
               | I_P8 | I_A7 | I_d9
               | I_m9 | I_A8
               | I_M9 | I_d10
               | I_m10 | I_A9
               | I_M10 | I_d11
               | I_P11 | I_A10
               | I_d12 | I_A11
               | I_P12 | I_d13
               | I_m13 | I_A12
               | I_M13 | I_d14
               | I_m14 | I_A13
               | I_M14 | I_d15
               | I_P15 | I_A14
               | I_A15
                 deriving (Eq, Ord, Show, Enum, Bounded)

-- mapping diatonic intervals to intervals (26 -> 40)
di_i :: DInterval -> Interval
di_i I_P1 = I Perfect Unison
di_i I_d2 = I Diminished Second
di_i I_m2 = I Minor Second
di_i I_A1 = I Augmented Unison
di_i I_M2 = I Major Second
di_i I_d3 = I Diminished Third
di_i I_m3 = I Minor Third
di_i I_A2 = I Augmented Second
di_i I_M3 = I Major Third
di_i I_d4 = I Diminished Fourth
di_i I_P4 = I Perfect Fourth
di_i I_A3 = I Augmented Third
di_i I_d5 = I Diminished Fifth
di_i I_A4 = I Augmented Fourth
di_i I_P5 = I Perfect Fifth
di_i I_d6 = I Diminished Sixth
di_i I_m6 = I Minor Sixth
di_i I_A5 = I Augmented Fifth
di_i I_M6 = I Major Sixth
di_i I_d7 = I Diminished Seventh
di_i I_m7 = I Minor Seventh
di_i I_A6 = I Augmented Sixth
di_i I_M7 = I Major Seventh
di_i I_d8 = I Diminished Octave
di_i I_P8 = I Perfect Octave
di_i I_A7 = I Augmented Seventh

-- PARTIAL FUNCTION
-- mapping semitones and diatonic numbers to diatonic intervals (104 -> 26)
q3 :: Y -> DiatonicNumber -> DInterval
q3 (Y 0) Unison   = I_P1
q3 (Y 0) Second   = I_d2
q3 (Y 1) Second   = I_m2
q3 (Y 1) Unison   = I_A1
q3 (Y 2) Second   = I_M2
q3 (Y 2) Third    = I_d3
q3 (Y 3) Third    = I_m3
q3 (Y 3) Second   = I_A2
q3 (Y 4) Third    = I_M3
q3 (Y 4) Fourth   = I_d4
q3 (Y 5) Fourth   = I_P4
q3 (Y 5) Third    = I_A3
q3 (Y 6) Fifth    = I_d5
q3 (Y 6) Fourth   = I_A4
q3 (Y 7) Fifth    = I_P5
q3 (Y 7) Sixth    = I_d6
q3 (Y 8) Sixth    = I_m6
q3 (Y 8) Fifth    = I_A5
q3 (Y 9) Sixth    = I_M6
q3 (Y 9) Seventh  = I_d7
q3 (Y 10) Seventh = I_m7
q3 (Y 10) Sixth   = I_A6
q3 (Y 11) Seventh = I_M7
q3 (Y 11) Octave  = I_d8
q3 (Y 0) Octave   = I_P8
q3 (Y 0) Seventh  = I_A7
-- 78 cases lead to error
--q3 y d = error $ "No " ++ show d ++ " covers " ++ show (unY y) ++ " semitones"
                 
-- mapping two pitch classes to a diatonic interval
q4 :: PitchClass -> PitchClass -> DInterval
q4 p1 p2 = q3 (mkY (pp_asc_y p1 p2)) (pp_dn p1 p2)

-- mapping two pitch classes to a interval
q5 :: PitchClass -> PitchClass -> Interval
q5 p1 p2 = di_i $ q4 p1 p2

-- mapping two pitch class labels to interval
q6 :: PCLabel -> PCLabel -> Interval
q6 p1 p2 = q5 (pcl_pc p1) (pcl_pc p2)














           


--------------------------------------------------------------------------------
-- Scales
--

-- a scale, expressed as steps relative to a tonic
-- data Scale = Monotonic   Y
--            | Ditonic     Y Y
--            | Tritonic    Y Y Y
--            | Tetratonic  Y Y Y Y
--            | Pentatonic  Y Y Y Y Y
--            | Hexatonic   Y Y Y Y Y Y
--            | Heptatonic  Y Y Y Y Y Y Y
--            | Octatonic   Y Y Y Y Y Y Y Y
--            | Nonatonic   Y Y Y Y Y Y Y Y Y
--            | Decatonic   Y Y Y Y Y Y Y Y Y Y
--            | Undecatonic Y Y Y Y Y Y Y Y Y Y Y
--            | Dodecatonic Y Y Y Y Y Y Y Y Y Y Y Y

-- -- extracting list of Y from a Scale
-- s_i :: Scale -> [Y]
-- s_i (Monotonic   a)                       = [a]
-- s_i (Ditonic     a b)                     = [a, b]
-- s_i (Tritonic    a b c)                   = [a, b, c]
-- s_i (Tetratonic  a b c d)                 = [a, b, c, d]
-- s_i (Pentatonic  a b c d e)               = [a, b, c, d, e]
-- s_i (Hexatonic   a b c d e f)             = [a, b, c, d, e, f]
-- s_i (Heptatonic  a b c d e f g)           = [a, b, c, d, e, f, g]
-- s_i (Octatonic   a b c d e f g h)         = [a, b, c, d, e, f, g, h]
-- s_i (Nonatonic   a b c d e f g h i)       = [a, b, c, d, e, f, g, h, i]
-- s_i (Decatonic   a b c d e f g h i j)     = [a, b, c, d, e, f, g, h, i, j]
-- s_i (Undecatonic a b c d e f g h i j k)   = [a, b, c, d, e, f, g, h, i, j, k]
-- s_i (Dodecatonic a b c d e f g h i j k l) = [a, b, c, d, e, f, g, h, i, j, k, l]


-- -- dodecatonic scales
-- chromatic      = Dodecatonic Y1 Y1 Y1 Y1 Y1 Y1 Y1 Y1 Y1 Y1 Y1 Y1

-- -- heptatonic scales
-- major           = Heptatonic Y2 Y2 Y1 Y2 Y2 Y2 Y1 -- Major_scale
-- harmonic_major  = Heptatonic Y2 Y2 Y1 Y2 Y1 Y3 Y1 -- Harmonic_major_scale
-- melodic_major   = Heptatonic Y2 Y2 Y1 Y2 Y1 Y2 Y2 -- Major_scale#Broader_sense
-- double_harmonic = Heptatonic Y1 Y3 Y1 Y2 Y1 Y1 Y3 -- Double_harmonic_scale
-- minor           = Heptatonic Y2 Y1 Y2 Y2 Y1 Y2 Y2 -- Minor_scale
-- harmonic_minor  = Heptatonic Y2 Y1 Y2 Y2 Y1 Y3 Y1 -- Minor_scale#Harmonic_minor_scale

-- -- hexatonic scales
-- whole_tone      = Hexatonic Y2 Y2 Y2 Y2 Y2 Y2

-- -- pentatonic scales
-- pentatonic      = Pentatonic Y2 Y2 Y3 Y2 Y3 -- Pentatonic_scale#Major_pentatonic_scale



-- -- does a scale cover a single complete octave?
-- s_check :: Scale -> Bool
-- s_check = (== 12) . sum . (map fromEnum) . s_i
          
-- -- list of all known scales
-- all_scales :: [Scale]
-- all_scales = [ -- pentatonic
--                pentatonic,
--                -- hexatonic
--                whole_tone,
--                -- heptatonic
--                major,
--                harmonic_major,
--                melodic_major,
--                double_harmonic,
--                minor,
--                harmonic_minor,
--                -- dodecatonic
--                chromatic
--              ]

-- zz :: PCLabel -> Scale -> [X]
-- zz tonic scale = scanl step (pcl_x tonic) (s_i scale)
--     where
--       step :: X -> Y -> X
--       step a b = toEnum $ mod d m
--           where
--             d = fromEnum a + fromEnum b
--             m = mm $ mkX 0
      
-- z2 :: PCLabel -> Scale -> [PCLabel]
-- z2 tonic scale = scanl const tonic (zz tonic scale)

-- z3 :: PCLabel -> [Letter]
-- z3 pcl = rotate n enums
--     where
--       n = fromEnum $ pcl_l pcl


a0 p1 p2 = (p1, p2, pp_q p1 p2)
           
a1 = [(PC l a) | l <- enums, a <- enums]
a2 = [a0 p1 p2 | p1 <- a1, p2 <- a1]



return []
runTests' = quickCheckWithResult stdArgs { maxSuccess = 1000 }
runTests = $forAllProperties runTests'
