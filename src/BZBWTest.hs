-- Author: Maciej Bendkowski
-- <maciej.bendkowski@tcs.uj.edu.pl>
module BZBWTest where
    
    import BZBW
    import Definitions
    import Generators()

    import Test.QuickCheck

    -- Ensures that bwtobz o bztobwb = id
    prop_rev_bwtobz :: BWTree -> Bool
    prop_rev_bwtobz t = bzToBwB (bwToBz t) == t

    -- Ensures that bztobwb o bwtobz = id
    prop_rev_bztobw :: BZTree -> Bool
    prop_rev_bztobw t = bwToBz (bzToBwB t) == t

    main :: IO ()
    main = do
        quickCheck prop_rev_bwtobz
        quickCheck prop_rev_bztobw
