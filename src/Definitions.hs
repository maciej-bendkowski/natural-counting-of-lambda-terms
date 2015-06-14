-- Author: Maciej Bendkowski
-- <maciej.bendkowski@tcs.uj.edu.pl>
module Definitions where
    
    -- DeBruijn index datatype.
    data DeBruijn = Z
                  | S DeBruijn
                    deriving (Eq,Show)

    -- λ-term datatype.
    data LTerm = App LTerm LTerm
               | Abs LTerm
               | Nat DeBruijn
                    deriving (Eq,Show)

    -- Black-White binary tree datatype.
    data BWTree = Black BWTree BWTree
                | White BWTree BWTree
                | Leaf
                    deriving (Eq,Show)

    -- Zigzag-free binary tree datatype.
    data BZTree = Node BZTree BZTree
                | BZLeaf
                    deriving (Eq,Show)
