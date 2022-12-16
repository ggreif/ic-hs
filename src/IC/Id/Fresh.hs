module IC.Id.Fresh where

import IC.Types
import IC.Id.Forms

import Data.ByteString.Builder
import Data.List
import Data.Word

-- Not particulary efficent, but this is a reference implementation, right?
freshId :: [(Word64, Word64)] -> [EntityId] -> Maybe EntityId
freshId ranges ids =
    case filter (`notElem` ids) $ map wordToId $ concatMap (\(a, b) -> [a..b]) ranges of
      [] -> Nothing
      (x:_) -> Just x

wordToId :: Word64 -> EntityId
wordToId = EntityId . mkOpaqueId . toLazyByteString . word64BE

checkCanisterIdInRanges :: [(Word64, Word64)] -> CanisterId -> Bool
checkCanisterIdInRanges ranges cid = find (\(a, b) -> wordToId a <= cid && cid <= wordToId b) ranges /= Nothing

isRootTestSubnet :: TestSubnetConfig -> Bool
isRootTestSubnet (_, _, _, ranges) = checkCanisterIdInRanges ranges nns_canister_id
  where
    nns_canister_id = wordToId 0
