-- Author: Maciej Bendkowski
-- <maciej.bendkowski@tcs.uj.edu.pl>
module BWLTest where
    
    import BWL
    import Definitions
    import Generators()

    import Test.QuickCheck

    -- Ensures that ltobw o bwtol = id
    prop_rev_ltobw :: LTerm -> Bool
    prop_rev_ltobw t = bwToL (lToBw t) == t

    -- Ensures that bwtol o ltobw = id
    prop_rev_bwtol :: BWTree -> Bool
    prop_rev_bwtol t = lToBw (bwToL t) == t

    main :: IO ()
    main = do
        quickCheck prop_rev_bwtol
        quickCheck prop_rev_ltobw
