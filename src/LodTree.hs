module LodTree where

import Data.Tree

test :: Tree (String, Double)
test = Node ("iss_L9", 1.0) [ Node ("iss_L8", 0.5) [ Node ("iss_S3_S4_L7", 0.25) []
                                                   , Node ("merge_iss_L6", 0.25) []
                                                   , Node ("iss_S5_S6_L7", 0.25) []
                                                   ]
                            ] 


toTreeString :: Tree (String, Double) -> Tree String
toTreeString = fmap (\(x,y) -> (x ++ ", " ++ ( show y) ))

-- | putStr . drawTree . toTreeString $ test
-- iss_L9, 1.0
-- |
-- `- iss_L8, 0.5
--    |
--    +- iss_S3_S4_L7, 0.25
--    |
--    +- merge_iss_L6, 0.25
--    |
--    `- iss_S5_S6_L7, 0.25

-- test :: Tree String
-- test = Node "iss_L9" [ Node "iss_L8" [ Node "iss_S3_S4_L7" [ Node "iss_S3_L6"      [ Node "iss_S3_L5"      [ Node "iss_S3_L4"      [ Node "iss_S3_L3"      [ Node "iss_S3_L2"      [ Node "iss_S3_L1"      [] ] ] ] ] ]
--                                                            , Node "iss_S4_L6"      [ Node "iss_S4_L5"      [ Node "iss_S4_L4"      [ Node "iss_S4_L3"      [ Node "iss_S4_L2"      [ Node "iss_S4_L1"      [] ] ] ] ] ]
--                                                            ]                                               
--                                      , Node "merge_iss_L6" [ Node "iss_S1_L6"      [ Node "iss_S1_L5"      [ Node "iss_S1_L4"      [ Node "iss_S1_L3"      [ Node "iss_S1_L2"      [ Node "iss_S1_L1"      [] ] ] ] ] ]
--                                                            , Node "iss_S2_L6"      [ Node "iss_S2_L5"      [ Node "iss_S2_L4"      [ Node "iss_S2_L3"      [ Node "iss_S2_L2"      [ Node "iss_S2_L1"      [] ] ] ] ] ]
--                                                            , Node "iss_ZVEZDA_L6"  [ Node "iss_ZVEZDA_L5"  [ Node "iss_ZVEZDA_L4"  [ Node "iss_ZVEZDA_L3"  [ Node "iss_ZVEZDA_L2"  [ Node "iss_ZVEZDA_L1"  [] ] ] ] ] ]
--                                                            , Node "iss_ZARYA_L6"   [ Node "iss_ZARYA_L5"   [ Node "iss_ZARYA_L4"   [ Node "iss_ZARYA_L3"   [ Node "iss_ZARYA_L2"   [ Node "iss_ZARYA_L1"   [] ] ] ] ] ]
--                                                            , Node "iss_UNITY_L6"   [ Node "iss_UNITY_L5"   [ Node "iss_UNITY_L4"   [ Node "iss_UNITY_L3"   [ Node "iss_UNITY_L2"   [ Node "iss_UNITY_L1"   [] ] ] ] ] ]
--                                                            , Node "iss_MRSB_L6"    [ Node "iss_MRSB_L5"    [ Node "iss_MRSB_L4"    [ Node "iss_MRSB_L3"    [ Node "iss_MRSB_L2"    [ Node "iss_MRSB_L1"    [] ] ] ] ] ]
--                                                            , Node "iss_DESTINY_L4" [ Node "iss_DESTINY_L3" [ Node "iss_DESTINY_L2" [ Node "iss_DESTINY_L1" [] ] ] ]
--                                                            , Node "iss_HARMONY_L6" [ Node "iss_HARMONY_L5" [ Node "iss_HARMONY_L4" [ Node "iss_HARMONY_L3" [ Node "iss_HARMONY_L2" [ Node "iss_HARMONY_L1" [] ] ] ] ] ]
--                                                            ]
--                                      , Node "iss_S5_S6_L7" [ Node "iss_S5_L6"      [ Node "iss_S5_L5"      [ Node "iss_S5_L4"      [ Node "iss_S5_L3"      [ Node "iss_S5_L2"      [ Node "iss_S5_L1"      [] ] ] ] ] ]
--                                                            , Node "iss_S6_L6"      [ Node "iss_S6_L5"      [ Node "iss_S6_L4"      [ Node "iss_S6_L3"      [ Node "iss_S6_L2"      [ Node "iss_S6_L1"      [] ] ] ] ] ]
--                                                            ]
--                                      ]
--                      ] 

-- test = Node { rootLabel = ("iss_L9")
--            , subForest = [ Node { rootLabel = ("iss_L8")
--                                 , subForest = [] }
--                          ] }
