-- Author: Maciej Bendkowski
-- <maciej.bendkowski@tcs.uj.edu.pl>
module BWL where
    
    import Definitions

    -- Substitutes the given Black-White tree bwt
    -- for the leftmost node in the second tree.
    sub :: BWTree -> BWTree -> BWTree
    sub bwt (Black t t') = Black (bwt `sub` t) t'
    sub bwt (White t t') = White (bwt `sub` t) t'
    sub bwt Leaf = bwt

    -- Translates the given DeBruijn index
    -- to a corresponding Black-White tree.
    dToBw :: DeBruijn -> BWTree
    dToBw Z = Black Leaf Leaf
    dToBw (S n) = Black Leaf Leaf `sub` dToBw n

    -- Translates the given Lambda-term to
    -- a corresponding Black-White tree.
    lToBw :: LTerm -> BWTree
    lToBw (Nat n) = dToBw n
    lToBw (Abs e) = White Leaf Leaf `sub` lToBw e
    lToBw (App e e') = White Leaf (lToBw e) `sub` lToBw e'

    -- Cuts the leftmost subtree out from the given tree
    -- returning a pair (leftmost subtree, pruned tree).
    prune :: BWTree -> (BWTree, BWTree)
    prune p @ (White Leaf _) = (p, Leaf)
    prune (White l r) = case prune l of
        (lm, p) -> (lm, White p r)
    prune p @ (Black Leaf _) = (p, Leaf)
    prune (Black l r) = case prune l of
        (lm, p) -> (lm, Black p r)

    -- Translates the given black rooted Black-White
    -- tree to a corresponding DeBruijn index.
    bToD :: BWTree -> DeBruijn
    bToD (Black Leaf Leaf) = Z
    bToD (Black t Leaf) = S $ bToD t

    -- Translates the given Black-White
    -- tree to a corresponding Lambda-term.
    bwToL :: BWTree -> LTerm
    bwToL bwt = case prune bwt of
        (Black Leaf Leaf, _) -> Nat $ bToD bwt
        (White Leaf Leaf, pt) -> Abs $ bwToL pt
        (White Leaf t, pt) -> App (bwToL t) (bwToL pt)
