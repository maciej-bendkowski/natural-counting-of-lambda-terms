-- Author: Maciej Bendkowski
-- <maciej.bendkowski@tcs.uj.edu.pl>
module Generators where
    
    import Control.Monad
    import Test.QuickCheck
    import Definitions

    -- Constructs the nth de Bruijn index.
    nthDeBruijn :: Integral a => a -> DeBruijn
    nthDeBruijn 0 = Z
    nthDeBruijn n = S $ nthDeBruijn (n - 1)

    -- Returns an arbitrary random λ-term.
    -- Note: This is *not* a uniformly random sampler.
    arbLTerm :: Integral a => a -> Gen LTerm
    arbLTerm 0 = return $ Nat Z
    arbLTerm n = frequency
        [(1, return $ Nat (nthDeBruijn n)),
         (1, liftM Abs $ arbLTerm (n - 1)),
         (1, liftM2 App (arbLTerm $ n `div` 2) (arbLTerm $ n `div` 2))]

    instance Arbitrary LTerm where
        arbitrary = sized arbLTerm

    -- Returns an arbitrary random black-white tree
    -- starting with a black root node. Note:
    -- This is *not* a uniformly random sampler.
    arbBWTreeB :: Integral a => a -> Gen BWTree
    arbBWTreeB 0 = return $ Black Leaf Leaf
    arbBWTreeB n = frequency
        [(1, liftM2 Black (arbBWTreeB $ n - 1) (return Leaf)),
        (1, liftM2 Black (arbBWTreeW $ n - 1) (return Leaf))]
    
    -- Returns an arbitrary random black-white tree
    -- starting with a white root node. Note:
    -- This is *not* a uniformly random sampler.
    arbBWTreeW :: Integral a => a -> Gen BWTree
    arbBWTreeW 0 = return $ White Leaf Leaf
    arbBWTreeW n = frequency
        [(1, liftM2 White (arbBWTreeW $ n - 1) (return Leaf)),
        (1, liftM2 White (return Leaf) (arbBWTreeB $ n - 1)),
        (1, liftM2 White (arbBWTreeW $ n `div` 2) (arbBWTreeB $ n `div` 2))]

    instance Arbitrary BWTree where
        arbitrary = sized arbBWTreeB

    -- Returns an arbitrary random zigzag-free tree
    -- from the class BZ_1. Note: This is *not* a 
    -- uniformly random sampler.
    arbBZTree :: Integral a => a -> Gen BZTree
    arbBZTree 0 = return $ Node BZLeaf BZLeaf
    arbBZTree n = frequency
        [(1, liftM2 Node (return BZLeaf) (arbBZTree $ n - 1)),
         (1, arbBZTree' n)]
    
    -- Returns an arbitrary random zigzag-free tree
    -- from the class BZ_2. Note: This is *not* a 
    -- uniformly random sampler.
    arbBZTree' :: Integral a => a -> Gen BZTree
    arbBZTree' 0 = return $ Node BZLeaf BZLeaf
    arbBZTree' n = frequency
        [(1, liftM2 Node (arbBZTree' $ n - 1) (return BZLeaf)),
         (1, liftM2 Node (arbBZTree' $ n `div` 2) (arbBZTree $ n `div` 2))]

    instance Arbitrary BZTree where
        arbitrary = sized arbBZTree
