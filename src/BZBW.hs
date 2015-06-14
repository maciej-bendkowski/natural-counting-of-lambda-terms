-- Author: Maciej Bendkowski
-- <maciej.bendkowski@tcs.uj.edu.pl>
module BZBW where
    
    import Definitions

    -- Translates the given Black-White tree
    -- to a corresponding Zigzag free tree.
    bwToBz :: BWTree -> BZTree
    bwToBz (Black Leaf Leaf) = Node BZLeaf BZLeaf
    bwToBz (Black t @ (Black _ _) Leaf) = Node BZLeaf (bwToBz t)
    bwToBz (Black t @ (White _ _) Leaf) = bwToBz t
    bwToBz (White Leaf Leaf) = Node (Node BZLeaf BZLeaf) BZLeaf
    bwToBz (White t @ (White _ _) Leaf) = Node (bwToBz t) BZLeaf
    bwToBz (White Leaf t @ (Black _ _)) = Node (Node BZLeaf BZLeaf) (bwToBz t)
    bwToBz (White t t') = Node (bwToBz t) (bwToBz t')

    -- Translates the given Zigzag free tree to a
    -- corresponding black rooted Black-White tree.
    bzToBwB :: BZTree -> BWTree
    bzToBwB (Node BZLeaf BZLeaf) = Black Leaf Leaf
    bzToBwB (Node BZLeaf t @ (Node _ _)) = Black (bzToBwB t) Leaf
    bzToBwB (Node (Node BZLeaf BZLeaf) BZLeaf) = bzToBwB' Leaf Leaf
    bzToBwB (Node t BZLeaf) = bzToBwB' (bzToBwW t) Leaf
    bzToBwB (Node (Node BZLeaf BZLeaf) t') = bzToBwB' Leaf (bzToBwB t')
    bzToBwB (Node t t') = bzToBwB' (bzToBwW t) (bzToBwB t')
  
    -- Helper function.
    bzToBwB' :: BWTree -> BWTree -> BWTree
    bzToBwB' x y = Black (White x y) Leaf

    -- Translates the given Zigzag free tree to a
    -- corresponding white rooted Black-White tree.
    bzToBwW :: BZTree -> BWTree
    bzToBwW (Node (Node BZLeaf BZLeaf) BZLeaf) = White Leaf Leaf
    bzToBwW (Node (Node BZLeaf BZLeaf) t) = White Leaf (bzToBwB t)
    bzToBwW (Node t BZLeaf) = White (bzToBwW t) Leaf
    bzToBwW (Node t t') = White (bzToBwW t) (bzToBwB t')
