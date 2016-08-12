{-# LANGUAGE TupleSections, GeneralizedNewtypeDeriving, ViewPatterns #-}

module Problems
  ( module ProblemsExports
  , getResult
    -- ** Problem maps
  , problemMap, answerMap
    -- ** Problem IDs
  , PbId, mkPbId
  , lookupId, lookupIds
  ) where

import Computation   as ProblemsExports (Computation, PbReport, Result, getMResult, getMessage)

import Computation
import PbImports

import Data.Either
import Data.IntMap.Lazy as Map

-- | Problem ID.
newtype PbId = PbId Int
  deriving (Enum, Eq, Ord, Num, Real, Integral)

instance Show PbId where
  show (PbId res) = show res
  showsPrec d (PbId res) = showsPrec d res

-- | Smart constructor for IDs.
mkPbId :: (Integral a) => a -> Maybe PbId
mkPbId (toInteger -> n)
  | inRange n = Just . PbId . fromInteger $ n
  | otherwise = Nothing
  where inRange m = m <= toInteger (maxBound :: Int)

-- | Look up an ID in a map.
lookupId :: IntMap a -> PbId -> Maybe a
lookupId intMap (PbId i) = Map.lookup i intMap

-- | Look up a list of IDs in a map.
lookupIds :: IntMap a -> [PbId]
          -> ([PbId], [(PbId, a)])  -- ^ List of IDs absent from the map and
                                    --   list of (ID, Item) pairs successfully retrieved.
lookupIds intMap = partitionEithers . fmap tryLookupId
  where tryLookupId pbId@(lookupId intMap -> Just a) = Right (pbId, a)
        tryLookupId pbId                             = Left pbId

-- | `Result` getter for `PbReport`.
-- This getter may fail.
getResult :: PbId -> PbReport -> Result
getResult _ (getMResult -> Just res) = res
getResult i  _                       = error $ "getReportResult: problem "++ show i ++" has no result value"

-- | Map of the problems.
problemMap :: IntMap Computation
problemMap = fromList
  [ (1, pb1), (2, pb2), (3, pb3), (4, pb4)
  , (5, pb5), (6, pb6), (7, pb7), (8, pb8), (9, pb9)
  , (10, pb10), (11, pb11), (12, pb12), (13, pb13), (14, pb14)
  , (15, pb15), (16, pb16), (17, pb17), (18, pb18), (19, pb19)
  , (20, pb20), (21, pb21), (22, pb22), (23, pb23), (24, pb24)
  , (25, pb25), (26, pb26), (27, pb27), (28, pb28), (29, pb29)
  , (30, pb30), (31, pb31), (32, pb32), (33, pb33), (34, pb34)
  , (35, pb35), (36, pb36), (37, pb37), (38, pb38), (39, pb39)
  , (40, pb40), (41, pb41), (42, pb42), (43, pb43), (44, pb44)
  , (45, pb45), (46, pb46), (47, pb47), (48, pb48), (49, pb49)
  , (50, pb50), (51, pb51), (52, pb52), (53, pb53), (54, pb54)
  , (55, pb55), (56, pb56), (57, pb57), (58, pb58), (59, pb59)
  , (60, pb60), (61, pb61), (62, pb62), (63, pb63), (64, pb64)
  , (65, pb65), (66, pb66), (67, pb67), (68, pb68), (69, pb69)
   {-, (70, pb70) -}, (71, pb71), (72, pb72), (73, pb73), (74, pb74)
  , (75, pb75), (76, pb76), (77, pb77), (78, pb78) {-, (79, pb79) -}
  , (80, pb80), (81, pb81), (82, pb82), (83, pb83), (84, pb84)
  , (85, pb85), (86, pb86), (87, pb87), (88, pb88), (89, pb89)
  , (90, pb90), (91, pb91), (92, pb92), (93, pb93), (94, pb94)
  , (95, pb95), (96, pb96), (97, pb97), (98, pb98), (99, pb99)
  , (100, pb100), (101, pb101), (102, pb102) {-, (103, pb103) -}, (104, pb104)
  , (105, pb105) {-, (106, pb106) -}, (107, pb107), (108, pb108), (109, pb109)
  , (110, pb110), (111, pb111), (112, pb112), (113, pb113), (114, pb114)
  , (115, pb115), (116, pb116), (117, pb117), (118, pb118), (119, pb119)
  , (120, pb120)
  , (125, pb125)
  , (151, pb151)
  ]

-- | Map of the problem answers.
answerMap :: IntMap Result
answerMap = fromList . fmap (Result <$>) $
  [ (1, 233168), (2, 4613732), (3, 6857), (4, 906609)
  , (5, 232792560), (6, 25164150), (7, 104743), (8, 23514624000), (9, 31875000)
  , (10, 142913828922), (11, 70600674), (12, 76576500), (13, 5537376230), (14, 837799)
  , (15, 137846528820), (16, 1366), (17, 21124), (18, 1074), (19, 171)
  , (20, 648), (21, 31626), (22, 871198282), (23, 4179871), (24, 2783915460)
  , (25, 4782), (26, 983), (27, -59231), (28, 669171001), (29, 9183)
  , (30, 443839), (31, 73682), (32, 45228), (33, 100), (34, 40730)
  , (35, 55), (36, 872187), (37, 748317), (38, 932718654), (39, 840)
  , (40, 210), (41, 7652413), (42, 162), (43, 16695334890), (44, 5482660)
  , (45, 1533776805), (46, 5777), (47, 134043), (48, 9110846700), (49, 296962999629)
  , (50, 997651), (51, 121313), (52, 142857), (53, 4075), (54, 376)
  , (55, 249), (56, 972), (57, 153), (58, 26241), (59, 107359)
  , (60, 26033), (61, 28684), (62, 127035954683), (63, 49), (64, 1322)
  , (65, 272), (66, 661), (67, 7273), (68, 6531031914842725), (69, 510510)
  , (70, 8319823), (71, 428570), (72, 303963552391), (73, 7295372), (74, 402)
  , (75, 161667), (76, 190569291), (77, 71), (78, 55374), (79, 73162890)
  -- , (d0, ), (d1, ), (d2, ), (d3, ), (d4, )
  -- , (d5, ), (d6, ), (d7, ), (d8, ), (d9, )
  ]
